-module(erltity_epgsql_example_conf).

%%% EXTERNAL EXPORTS
-export([
    host/0,
    username/0,
    password/0,
    database/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec host() -> Res when
    Res :: undefined | list().
host() ->
    application:get_env(erltity_epgsql_example, host, undefined).

-spec username() -> Res when
    Res :: undefined | list().
username() ->
    application:get_env(erltity_epgsql_example, username, undefined).

-spec password() -> Res when
    Res :: undefined | list().
password() ->
    application:get_env(erltity_epgsql_example, password, undefined).

-spec database() -> Res when
    Res :: undefined | list().
database() ->
    application:get_env(erltity_epgsql_example, database, undefined).
