%%%-------------------------------------------------------------------
%% @doc obran_hipaa top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(obran_hipaa_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpecs = [{phi_backend_handler, {phi_backend_handler, start_link, []}, permanent, 5000, worker, [phi_backend_handler]}],
    {ok, {SupFlags, ChildSpecs}}.
