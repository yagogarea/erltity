-module(erltity_epgsql_tests).

%%% INCLUDE FILES
-include_lib("eunit/include/eunit.hrl").

%%%-----------------------------------------------------------------------------
%%% TESTS DESCRIPTIONS
%%%-----------------------------------------------------------------------------
crud_test_() ->
    {"Ensure that the CRUD operations works",
        [fun start/0, fun test_crud/0, fun stop/0]}.

%%%-----------------------------------------------------------------------------
%%% SETUP FUNCTION
%%%-----------------------------------------------------------------------------
start() ->
    application:ensure_all_started(erltity),
    application:ensure_all_started(epgsql),
    erltity:start_link(epgsql,
        #{
            host => "localhost",
            username => "postgres",
            password => "postgres",
            database => "epgsql_test"
        }
    ).

stop() ->
    epgsql:squery(persistent_term:get(epgsql_connection), "DROP TABLE dog"),
    erltity_epgsql:stop(stop).

%%%-----------------------------------------------------------------------------
%%% TESTS
%%%-----------------------------------------------------------------------------
test_crud() ->
    DogCreateRequest = #{<<"name">> => <<"Nala">>, <<"breed">> => <<"Chihuahua">>},
    Dog = #{<<"id">> => 1, <<"name">> => <<"Nala">>, <<"breed">> => <<"Chihuahua">>},
    UpdatedDog = #{<<"id">> => 1, <<"name">> => <<"Nala">>, <<"breed">> => <<"Doberman">>},
    ?assertMatch(ok, erltity:register(<<"test/schemas/dog.json">>, #{})),
    ?assertMatch({ok, Dog}, dog:create(DogCreateRequest)),
    ?assertMatch({error, not_found}, dog:find(0)),
    ?assertMatch({error, not_found}, dog:update(0, #{<<"breed">> => <<"Doberman">>})),
    ?assertMatch({ok, Dog}, dog:find(1)),
    ?assertMatch(ok, dog:update(1, #{<<"breed">> => <<"Doberman">>})),
    ?assertMatch({ok, UpdatedDog}, dog:find(1)),
    ?assertMatch(ok, dog:delete(1)).