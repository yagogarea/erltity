-module(erltity_sup).

%%% BEHAVIOUR
-behaviour(supervisor).

%%% EXTERNAL EXPORTS
-export([
    start_link/2
]).

%%% SUPERVISOR BEHAVIOUR CALLBACKS
-export([
    init/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link(DbDriver, Conf) -> Result when
    DbDriver :: erltity:db_driver(),
    Conf :: erltity:opts(),
    Result :: supervisor:startlink_ret().
start_link(DbDriver, Conf) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [DbDriver, Conf]).

%%%-----------------------------------------------------------------------------
%%% SUPERVISOR BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
init([DbDriver, Conf]) ->
    Module = list_to_atom("erltity_" ++ atom_to_list(DbDriver)),
    persistent_term:put(db_driver_module, Module),
    Flags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 5
    },
    DbDriverChild = #{
        id => Module,
        start => {Module, start_link, [Conf]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [Module]
    },
    {ok, {Flags, [DbDriverChild]}}.
