-module(erltity_epgsql_example_sup).

%%% BEHAVIOUR
-behaviour(supervisor).

%%% EXTERNAL EXPORTS
-export([
    start_link/0
]).

%%% SUPERVISOR BEHAVIOUR CALLBACKS
-export([
    init/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link() -> Result when
    Result :: supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%-----------------------------------------------------------------------------
%%% SUPERVISOR BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
init([]) ->
    DbDriver = epgsql,
    Flags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 5
    },
    Host = erltity_epgsql_example_conf:host(),
    Username = erltity_epgsql_example_conf:username(),
    Password = erltity_epgsql_example_conf:password(),
    Database = erltity_epgsql_example_conf:database(),
    ErltityChild = #{
        id => erltity,
        start => {erltity, start_link, [
            DbDriver,
            #{
                host => Host,
                username => Username,
                password => Password,
                database => Database
            }
        ]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [erltity]
    },
    {ok, {Flags, [ErltityChild]}}.
