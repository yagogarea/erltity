-module(erltity_epgsql_sup).

%%% BEHAVIOUR
-behaviour(supervisor).

%%% EXTERNAL EXPORTS
-export([
    start_link/1
]).

%%% SUPERVISOR BEHAVIOUR CALLBACKS
-export([
    init/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link(Opts) -> Result when
    Opts :: epgsql:connect_opts(),
    Result :: supervisor:startlink_ret().
start_link(Opts) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Opts]).

%%%-----------------------------------------------------------------------------
%%% SUPERVISOR BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
init([Opts]) ->
    Flags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 5
    },
    {ok, Conn} = epgsql:connect(Opts),
    persistent_term:put(epgsql_connection, Conn),
    {ok, {Flags, []}}.
