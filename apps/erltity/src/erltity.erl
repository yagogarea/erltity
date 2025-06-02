-module(erltity).

%%% START/STOP EXPORTS
-export([
    start_link/2,
    stop/1
]).

%%% EXTERNAL EXPORTS
-export([
    register/2,
    create/2,
    update/3,
    delete/2,
    find/2,
    find/3
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

-type schemas_path() :: binary().

-type cursor() :: binary() | undefined.

-type find_opts() :: #{
    page => non_neg_integer(),
    page_size => non_neg_integer(),
    offset => non_neg_integer()
}.

%%% EXPORT TYPES
-export_type([
    opts/0,
    db_driver/0,
    schemas_path/0,
    register_opts/0,
    find_opts/0,
    cursor/0
]).

%%%-----------------------------------------------------------------------------
%%% CALLBACKS
%%%-----------------------------------------------------------------------------
-callback create(CreateRequest) -> Result when
    CreateRequest :: map(),
    Result :: {ok, Entity} | {error, Reason},
    Entity :: map(),
    Reason :: term().

-callback update(Id, UpdateRequest) -> Result when
    UpdateRequest :: map(),
    Id :: term(),
    Result :: {ok, Entity} | {error, Reason},
    Entity :: map(),
    Reason :: term().

-callback delete(Id) -> Result when
    Id :: term(),
    Result :: ok | {error, Reason},
    Reason :: term().

-callback find(Id) -> Result when
    Id :: term(),
    Result :: {ok, Entity} | {error, Reason},
    Entity :: map(),
    Reason :: term().

-callback find(Filters, Opts) -> Result when
    Filters :: map(),
    Opts :: find_opts(),
    Result :: {ok, [Entity]} | {error, Reason},
    Entity :: map(),
    Reason :: term().

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

-spec create(EntityName, CreateRequest) -> Result when
    EntityName :: atom(),
    CreateRequest :: map(),
    Result :: {ok, Entity} | {error, Reason},
    Entity :: map(),
    Reason :: term().
%% @doc Saves a new entity of type `EntityName` with the given `CreateRequest`.
create(EntityName, CreateRequest) ->
    EntityName:create(CreateRequest).

-spec update(EntityName, Id, UpdateRequest) -> Result when
    EntityName :: atom(),
    UpdateRequest :: map(),
    Id :: term(),
    Result :: {ok, Entity} | {error, Reason},
    Entity :: map(),
    Reason :: term().
%% @doc Updates an existing entity of type `EntityName` with the given `Id` and `UpdateRequest`.
update(EntityName, Id, UpdateRequest) ->
    EntityName:update(Id, UpdateRequest).

-spec delete(EntityName, Id) -> Result when
    EntityName :: atom(),
    Id :: term(),
    Result :: ok | {error, Reason},
    Reason :: term().
%% @doc Deletes an existing entity of type `EntityName` with the given `Id`.
delete(EntityName, Id) ->
    EntityName:delete(Id).

-spec find(EntityName, Id) -> Result when
    EntityName :: atom(),
    Id :: term(),
    Result :: {ok, Entity} | {error, Reason},
    Entity :: map(),
    Reason :: term().
%% @doc Finds an entity of type `EntityName` with the given `Id`.
find(EntityName, Id) ->
    EntityName:find(Id).

-spec find(EntityName, Filters, Opts) -> Result when
    EntityName :: atom(),
    Filters :: map(),
    Opts :: find_opts(),
    Result :: {ok, [Entity]} | {error, Reason},
    Entity :: map(),
    Reason :: term().
%% @doc Finds entities of type `EntityName` that match the given `Filters` and `Opts`,
%% `Opts` can include pagination options like `page`, `page_size`, and `offset`.
find(EntityName, Filters, Opts) ->
    EntityName:find(Filters, Opts).


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
