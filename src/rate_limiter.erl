-module(rate_limiter).

-export([check_rate_limit/1]).

-include("defines.hrl").
-include("records.hrl").

check_rate_limit(UserName) ->
    case ets:lookup(?ETS_USER_DETAILS, UserName) of
        [] ->
            {allowed, []};
        [{_, UserData}] ->
            allow_or_block(UserName, UserData)
    end.

allow_or_block(UserName, UserData) ->
    CurrentTime = erlang:system_time(second),
    LastAttemptTime = UserData#user_rec.last_attempt_time,
    case CurrentTime - LastAttemptTime of
        T when T >= ?RATE_LIMIT_TIME ->  % Reset rate limit window
            UpdatedUserData = UserData#user_rec{login_attempts = 0, last_attempt_time = 0},
            ets:insert(?ETS_USER_DETAILS, {UserName, UpdatedUserData}),
            {allowed, UpdatedUserData};
        _ ->
            Attempts = UserData#user_rec.login_attempts,
            case Attempts < ?MAX_LOGIN_ATTEMPTS of
                true -> {allowed, UserData};
                _ ->
                    UpdatedUserData = UserData#user_rec{login_success = false},
                    ets:insert(?ETS_USER_DETAILS, {UserName, UpdatedUserData}),
                    blocked
            end
    end.
