%%%-------------------------------------------------------------------
%% @doc obran_hipaa public API
%% @end
%%%-------------------------------------------------------------------

-module(obran_hipaa_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    obran_hipaa_sup:start_link().

stop(_State) ->
    ok.
