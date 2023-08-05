-module(db).

-export([create_tables/0,
         delete_table/0]).

-include("defines.hrl").

create_tables() ->
    {ok, TableList} = erlcloud_ddb2:list_tables(),
    case lists:member(?PHI_TABLE_NAME, TableList) of
        true -> ok;
         _ ->
            erlcloud_ddb2:create_table(?PHI_TABLE_NAME, [{<<"userName">>, s}], <<"userName">>, 5, 5)
    end,
    case ets:info(?ETS_USER_DETAILS) of
        undefined ->
            ets:new(?ETS_USER_DETAILS, [set, named_table]);
        _ ->
            ok
    end.

delete_table() ->
    erlcloud_ddb2:delete_table(?PHI_TABLE_NAME).
