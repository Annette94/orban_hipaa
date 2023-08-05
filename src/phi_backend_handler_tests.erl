-module(phi_backend_handler_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test login with valid credentials
login_test() ->
    ?assertEqual({ok, <<"User added successfully">>}, phi_backend_handler:login(<<"user1">>, <<"password1">>, <<"admin">>, <<"PHIData">>)),
    ?assertEqual({ok, <<"Login successful">>}, phi_backend_handler:login(<<"user1">>, <<"password1">>, <<"admin">>, <<"PHIData">>)),
    ?assertEqual({ok, <<"Login successful">>}, phi_backend_handler:login(<<"user1">>, <<"password1">>, <<"admin">>, <<"PHIData">>)).

%% Test exporting PHI report for an authorized user
export_phi_report_test() ->
    ?assertEqual([{<<"user1">>,<<"admin">>,<<"PHIData">>}], phi_backend_handler:export_phi_report(<<"user1">>)).

%% Test login with invalid credentials
login_invalid_test() ->
    ?assertEqual({error, <<"Incorrect password">>}, phi_backend_handler:login(<<"user1">>, <<"wrong_password">>, <<"admin">>, <<"PHIData">>)),
    ?assertEqual({error, <<"Incorrect password">>}, phi_backend_handler:login(<<"user1">>, <<"wrong_password">>, <<"admin">>, <<"PHIData">>)),
    ?assertEqual({error, <<"Incorrect password">>}, phi_backend_handler:login(<<"user1">>, <<"wrong_password">>, <<"admin">>, <<"PHIData">>)).

%% Test rate-limiting behavior
login_rate_limit_test() ->
    ?assertEqual({error, <<"Rate limit reached. Try again later">>}, phi_backend_handler:login(<<"user1">>, <<"wrong_password">>, <<"admin">>, <<"PHIData">>)).

%% Test exporting PHI report for an unauthorized user
export_phi_report_unauthorized_test() ->
    ?assertEqual({error, <<"Unauthorized to export PHI report">>}, phi_backend_handler:export_phi_report("user3")).

run_test() ->
    {test, login_test},
    {test, export_phi_report_test},
    {test, login_invalid_test},
    {test, login_rate_limit_test},
    {test, export_phi_report_unauthorized_test}.
