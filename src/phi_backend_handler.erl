-module(phi_backend_handler).

-behaviour(gen_server).

-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([login/4,
         export_phi_report/1
        ]).

-include("defines.hrl").
-include("records.hrl").

-define(DEFAULT_TIMEOUT, 60).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(UserName, Password, Role, PHIData) ->
    gen_server:call(?MODULE, {login, UserName, Password, Role, PHIData}, ?DEFAULT_TIMEOUT).

export_phi_report(UserName) ->
    gen_server:call(?MODULE, {export_phi_report, UserName}, ?DEFAULT_TIMEOUT).

%% -----------------------------
init([]) ->
    erlcloud_ddb2:configure(?ACCESS_KEY_ID, ?SECRET_ACCCESS_KEY, "localhost", 8000,  "http://"),
    db:create_tables(),
    {ok, []}.

handle_call({login, UserName, Password, Role, PHIData}, _From, State) ->
    case rate_limiter:check_rate_limit(UserName) of
        blocked ->
            {reply, {error, <<"Rate limit reached. Try again later">>}, State};
        {allowed, UserDataRec} ->
            case is_new_user(UserName) of
                true ->  % New user, add to the database
                    add_user(UserName, Password, Role, PHIData),
                    {reply, {ok, <<"User added successfully">>}, State};
                false -> % Existing user, check password
                    case check_password(UserName, Password) of
                        true ->
                            update_login_success(UserName, UserDataRec),
                            {reply, {ok, <<"Login successful">>}, State};
                        false ->
                            increment_login_attempts(UserName, UserDataRec),
                            {reply, {error, <<"Incorrect password">>}, State}
                    end
            end
    end;
handle_call({export_phi_report, UserName}, _From, State) ->
    case check_role(UserName) of
        {<<"admin">>, true} ->
            Result = get_all_phi_data(UserName),
            {reply, Result, State};
        _ ->
            {reply, {error, <<"Unauthorized to export PHI report">>}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ======================
%% Internal functions
%% ======================
is_new_user(UserName) ->
    case ets:lookup(?ETS_USER_DETAILS, UserName) == [] of
        true ->
            case erlcloud_ddb2:get_item(?PHI_TABLE_NAME, {<<"userName">>, UserName}) of
                {ok, []} ->
                    true;
                {ok, Data} ->
                    Role = proplists:get_value(<<"role">>, Data),
                    UserData = #user_rec{user_name = UserName, role = Role, login_attempts = 0, last_attempt_time = 0, login_success = false},
                    ets:insert(?ETS_USER_DETAILS, {UserName, UserData}),
                    false
            end;
        false ->
            false
    end.

add_user(UserName, Pass, Role, PHIData) ->
    HashedPass = crypto_utils:hash_password(Pass),
    B64encodedData = base64:encode(PHIData),
    {EncryptedData, Tag} = crypto_utils:encrypt_aes(?Key, B64encodedData),
    % Store in DynamoDB
    Item = [{<<"userName">>, {s, UserName}},
            {<<"pass">>, {b, HashedPass}},
            {<<"role">>, {s, Role}},
            {<<"phidata">>, {b, EncryptedData}},
            {<<"tag">>, {b, Tag}}],
    {ok, _} = erlcloud_ddb2:put_item(?PHI_TABLE_NAME, Item),
    % Also store login information in ETS
    UserData = #user_rec{user_name = UserName, role = Role, login_attempts = 0, last_attempt_time = 0, login_success = true},
    ets:insert(?ETS_USER_DETAILS, {UserName, UserData}),
    ok.

check_password(UserName, Password) ->
    case erlcloud_ddb2:get_item(?PHI_TABLE_NAME, {<<"userName">>, UserName}) of
        {ok, Item} ->
            HashedPass = proplists:get_value(<<"pass">>, Item),
            crypto_utils:verify_password(Password, HashedPass);
        _ ->
            false
    end.

update_login_success(UserName, []) ->
    case ets:lookup(?ETS_USER_DETAILS, UserName) of
        [] ->
            lager:error("User Info not found");
        [{_, UserData}] ->
            update_login_success(UserName, UserData)
    end;
update_login_success(UserName, UserData) ->
    UpdatedUserData = UserData#user_rec{login_success = true},
    ets:insert(?ETS_USER_DETAILS, {UserName, UpdatedUserData}).

increment_login_attempts(UserName, []) ->
    case ets:lookup(?ETS_USER_DETAILS, UserName) of
        [] ->
            lager:error("User Info not found to update login attempts");
        [{_, UserData}] ->
            increment_login_attempts(UserName, UserData)
    end;
increment_login_attempts(UserName, UserData) ->
    CurrentTime = erlang:system_time(second),
    UpdatedUserData = UserData#user_rec{login_attempts = UserData#user_rec.login_attempts + 1, last_attempt_time = CurrentTime},
    ets:insert(?ETS_USER_DETAILS, {UserName, UpdatedUserData}).

check_role(UserName) ->
    case ets:lookup(?ETS_USER_DETAILS, UserName) of
        [] -> {undefined, false};
        [{_, UserData}] -> {UserData#user_rec.role, UserData#user_rec.login_success}
    end.

get_all_phi_data(_UserName) ->
    {ok, Items} = erlcloud_ddb2:scan(?PHI_TABLE_NAME),
    lists:map(fun extract_phi_data/1, Items).

extract_phi_data(Item) ->
    UserName = proplists:get_value(<<"userName">>, Item, undefined),
    Role = proplists:get_value(<<"role">>, Item, undefined),
    DecryptedData = crypto_utils:decrypt_aes(?Key, proplists:get_value(<<"phidata">>, Item), proplists:get_value(<<"tag">>, Item)),
    {UserName, Role, base64:decode(DecryptedData)}.
