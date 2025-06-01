-module(erltity_epgsql_example).

%%% BEHAVIOUR
-behaviour(application).

%%% START/STOP EXPORTS
-export([
    start/2,
    stop/1
]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
-spec start(StartType, StartArgs) -> Res when
    StartType :: term(),
    StartArgs :: term(),
    Res :: supervisor:startlink_ret().
start(_StartType, _StartArgs) ->
    erltity_epgsql_example_sup:start_link(),
    erltity:register(<<"schemas/dog.json">>, #{}).

-spec stop(StopArg) -> Res when
    StopArg :: term(),
    Res :: ok.
stop(_St) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL  EXPORTS
%%%-----------------------------------------------------------------------------
