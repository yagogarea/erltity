-module(erltity).

%%% START/STOP EXPORTS
-export([
    start_link/2,
    stop/1
]).

%%% EXTERNAL EXPORTS
-export([
    register/2
]).

%%% TYPES
-type opts() :: #{
    host => string(),
    username => string(),
    password => string(),
    database => string()
}.

-type register_opts() :: #{
    save_module => path(), % If no path is given it will not be saved.
    save_create_table => path() % Creates the table if none exists and save it if a valid path is provided.
}.

-type path() :: binary().

-type db_driver() :: epgsql.

-type schemas_path() :: binary.

%%% EXPORT TYPES
-export_type([
    opts/0,
    db_driver/0,
    schemas_path/0,
    register_opts/0
]).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link(DbDriver, Opts) -> Result when
    DbDriver :: db_driver(),
    Opts :: opts(),
    Result :: supervisor:startlink_ret().
%% @doc Starts the erltity application connecting to the database using the given driver and options.
start_link(DbDriver, Opts) ->
    erltity_sup:start_link(DbDriver, Opts).

-spec stop(StopArg) -> Result when
    StopArg :: term(),
    Result :: ok.
%% @doc Stops the erltity application.
stop(_St) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec register(SchemaPath, Opts) -> Result when
    SchemaPath :: binary(),
    Opts :: register_opts(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Registers a schema from the given path and generates the new Erlang module.
register(SchemaPath, Opts) ->
    {SchemaName, Schema} = erltity_parser:parse(SchemaPath),
    DbDriverModule = persistent_term:get(db_driver_module),
    DbDriverModule:register(SchemaName, Schema, Opts),
    Forms = erltity_generator:generate(DbDriverModule, SchemaName),
    load(Forms),
    case maps:get(save_module, Opts, undefined) of
        undefined ->
            ok;
        Path ->
            {ok, File} = file:open(binary_to_list(Path) ++ atom_to_list(SchemaName) ++ ".erl", [write]),
            io:format(File, "~s", [erl_prettypr:format(erl_syntax:form_list(Forms))])
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
load(Forms) ->
    case compile:forms(Forms) of
        {ok, ModuleName, Binary} ->
            code:load_binary(ModuleName, "z", Binary);
        {ok, ModuleName, Binary, _Warnings} ->
            code:load_binary(ModuleName, "z", Binary)
    end.
