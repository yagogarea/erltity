-module(erltity_epgsql).

%% START/STOP EXPORTS
-export([
    start_link/1,
    stop/1
]).

%% EXTERNAL EXPORTS
-export([
    register/3,
    create/2,
    update/3,
    delete/2,
    find/2,
    find/3
]).

%%% MACROS
-define(SEARCH_TABLE(Name), "SELECT * FROM pg_tables WHERE schemaname = 'public' AND tablename = '" ++ Name ++ "';").
-define(INSERT(Table, Keys, Values),
    "INSERT INTO "
    ++ Table
    ++ " "
    ++ Keys
    ++ " VALUES "
    ++ Values
    ++ " RETURNING *"
).
-define(UPDATE(TableName), "UPDATE " ++ atom_to_list(TableName) ++ " SET ").
-define(FIND(TableName), "SELECT * FROM " ++ atom_to_list(TableName)).
-define(FIND(TableName, ID), ?FIND(TableName) ++ " WHERE id = " ++ integer_to_list(ID)).
-define(DELETE(TableName, ID), "DELETE FROM " ++ atom_to_list(TableName) ++ " WHERE id = " ++ integer_to_list(ID)).

%%%-----------------------------------------------------------------------------
%%% START/STOP EXPORTS
%%%-----------------------------------------------------------------------------
-spec start_link(Opts) -> Res when
    Opts :: term(),
    Res :: supervisor:startlink_ret().
start_link(Opts) ->
    erltity_epgsql_sup:start_link(Opts).

-spec stop(StopArg) -> Res when
    StopArg :: term(),
    Res :: ok.
stop(_St) ->
    Connection = persistent_term:get(epgsql_connection),
    epgsql:close(Connection).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec register(Name, Schema, Opts) -> Result when
    Name :: atom(),
    Schema :: map(),
    Opts :: map(),
    Result :: ok | {error, Reason},
    Reason :: term().
register(Name, Schema, Opts) ->
    Connection = persistent_term:get(epgsql_connection),
    ListName = atom_to_list(Name),
    case epgsql:squery(Connection, ?SEARCH_TABLE(ListName)) of
        {ok, _Columns, []} ->
            case create_table(Name, Schema) of
                {error, Reason} ->
                    {error, Reason};
                Query ->
                    epgsql:squery(Connection, Query),
                    case maps:get(save_create_table, Opts, undefined) of
                        undefined ->
                            ok;
                        Path ->
                            {ok, File} = file:open(binary_to_list(Path) ++ ListName ++ ".sql", [write]),
                            io:format(File, "~s", [Query])
                    end
            end;
       {ok, _Columns, [_TableInfo]} -> % Table already exists
            ok
    end.

-spec create(TableName, CreateRequest) -> Result when
    TableName :: atom(),
    CreateRequest :: map(),
    Result  :: {ok, map()} | {error, Reason},
    Reason :: term().
create(TableName, CreateRequest) ->
    Connection = persistent_term:get(epgsql_connection),
    ListTableName = atom_to_list(TableName),
    {Keys, Values} = parse_keys(CreateRequest),
    Query = ?INSERT(ListTableName, Keys, query_parameters(length(Values))),
    case epgsql:equery(Connection, Query, Values) of 
        {ok, 1, Columns, [Result]} ->
            {ok, serialize_entity(Columns, Result)};
        Error ->
            Error
    end.

-spec update(TableName, Id, UpdateRequest) -> Result when
    TableName :: atom(),
    Id :: integer(),
    UpdateRequest :: map(),
    Result  :: ok | {error, Reason},
    Reason :: term().
update(TableName, Id, UpdateRequest) ->
    Connection = persistent_term:get(epgsql_connection),
    Query =  filters_to_query(maps:to_list(UpdateRequest), "', ", ?UPDATE(TableName)) ++ " WHERE id = " ++ integer_to_list(Id),
    case epgsql:equery(Connection, Query) of 
        {ok, 1} ->
            ok;
        {ok, 0} ->
            {error, not_found}
    end.

-spec delete(TableName, ID) -> Result when
    TableName :: atom(),
    ID :: integer(),
    Result  :: ok | {error, Reason},
    Reason :: term().
delete(TableName, ID) ->
    Connection = persistent_term:get(epgsql_connection),
    Query = ?DELETE(TableName, ID),
    case epgsql:equery(Connection, Query) of
        {ok, 0} ->
            {error, not_found};
        {ok, 1} ->
            ok
    end.

-spec find(TableName, ID) -> Result when
    TableName :: atom(),
    ID :: integer(),
    Result  :: ok | {error, Reason},
    Reason :: term().
find(TableName, ID) ->
    Connection = persistent_term:get(epgsql_connection),
    Query = ?FIND(TableName, ID),
    case epgsql:equery(Connection, Query) of
        {ok, _Columns, []} ->
            {error, not_found};
        {ok, Columns, [Result]} ->
            {ok, serialize_entity(Columns, Result)}
    end.

-spec find(TableName, Filters, Opts) -> Result when
    TableName :: atom(),
    Filters :: map(),
    Opts :: map(),
    Result  :: {ok, [map()]} | {error, Reason},
    Reason :: term().
find(TableName, Filters, _Opts) ->
    Connection = persistent_term:get(epgsql_connection),
    Query = filters_to_query(maps:to_list(Filters), "' AND ", ?FIND(TableName) ++ " WHERE "),
    case epgsql:equery(Connection, Query) of
        {ok, _Columns, []} ->
            {error, not_found};
        {ok, Columns, ResultList} ->
            {ok, lists:map(fun(Result) -> serialize_entity(Columns, Result) end, ResultList)}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------

serialize_entity(Columns, Values) ->
    serialize_entity(Columns, tuple_to_list(Values), #{}).

serialize_entity([], [], Acc) ->
    Acc;
serialize_entity([
    {
    column,
    Name,
    _Type,
    _Oid,
    _Size,
    _Modifier,
    _Format,
    _TableOid,
    _TableAttrNumber
    } | T1], [null | T2], Acc) ->
    serialize_entity(T1, T2, maps:put(Name, undefined, Acc));
serialize_entity([{
    column,
    Name,
    _Type,
    _Oid,
    _Size,
    _Modifier,
    _Format,
    _TableOid,
    _TableAttrNumber
    } | T1], [Value | T2], Acc) ->
    serialize_entity(T1, T2, maps:put(Name, Value, Acc)).

parse_keys(Map) ->
    parse_keys(maps:to_list(Map), {"", []}).

parse_keys([], {Acc1, Acc2}) ->
    {"(" ++ Acc1 ++ ")", lists:reverse(Acc2)};
parse_keys([{Key, Value}], {Acc1, Acc2}) ->
    parse_keys([], {Acc1 ++ binary_to_list(Key), [parse_item(Value) | Acc2]});
parse_keys([{Key, Value} | T], {Acc1, Acc2}) ->
    parse_keys(T, {Acc1 ++ binary_to_list(Key)++ ", ", [parse_item(Value) | Acc2]}).

parse_item(Item) when is_binary(Item) ->
    binary_to_list(Item);
parse_item(Item) when is_list(Item) ->
    Item.

query_parameters(N) ->
    query_parameters(N - 1, "$" ++ integer_to_list(N)).

query_parameters(0, Acc) ->
    "(" ++ Acc ++ ")";
query_parameters(1, Acc) ->
    query_parameters(0, "$1, " ++ Acc);
query_parameters(N, Acc) ->
    query_parameters(N - 1, "$" ++ integer_to_list(N) ++ ", " ++ Acc).

create_table(TableName, Schema) ->
    Properties = maps:to_list(maps:get(properties, Schema)),
    Requiered = maps:get(required, Schema),
    create_table_properties(Properties, Requiered, "CREATE TABLE " ++ atom_to_list(TableName) ++ " (\n\t").

create_table_properties([], [], Acc) ->
    Acc;
create_table_properties([{<<"id">>, Value} | T], Requiered, Acc) ->
    case maps:get(type, Value, undefined) of
        integer ->
            create_table_properties(T, Requiered, string:concat(Acc, "id SERIAL PRIMARY KEY,\n\t"));
        _Else ->
            {error, id_must_be_of_integer_type}
    end;
create_table_properties([{PropertyName, Value}], Requiered, Acc) ->
    Query = parse_property(PropertyName, Value, Requiered) ++ "\n);\n",
    string:concat(Acc, Query);
create_table_properties([{PropertyName, Value} | T], Requiered, Acc) ->
    Query = parse_property(PropertyName, Value, Requiered) ++ ",\n\t",
    create_table_properties(T, Requiered, string:concat(Acc, Query)).

parse_property(PropertyName, Value, Requiered) ->
    Type = parse_type(maps:get(type, Value), Value),
    case lists:member(PropertyName, Requiered) of
        true ->
            binary_to_list(PropertyName) ++ " " ++ Type ++ " NOT NULL";
        false ->
            binary_to_list(PropertyName) ++ " " ++ Type ++ " NULL"
    end.

parse_type(integer, _Value) ->
    "INT";
parse_type(boolean, _Value) ->
    "BOOLEAN";
parse_type(string, Value) ->
    case maps:get(content_encoding, Value, undefined) of
        <<"binary">> ->
            "BYTEA";
        _ ->
            parse_string(Value)
    end;
parse_type(array, Value) -> %% TO DO: Handle prefixItems, additional_items, unique_items
    case maps:get(items, Value, undefined) of
        undefined ->
            {error, array_items_not_defined};
        false ->
            "";
        Items ->
            case maps:get(type, Items, undefined) of
                undefined ->
                    {error, array_items_type_not_defined};
                Type ->
                    parse_type(Type, Items) ++ "[]"
            end
    end.

parse_string(Value) ->
    case {maps:get(min_length, Value, undefined), maps:get(max_length, Value, undefined)} of
        {undefined, undefined} ->
            "TEXT";
        {64, 64} ->
            "NAME";
        {Length, Length} ->
            "CHAR(" ++ integer_to_list(Length) ++ ")";
        {_MinLength, MaxLength} ->
            "VARCHAR(" ++ integer_to_list(MaxLength) ++ ")"
    end.

filters_to_query([], _Separator, Acc) ->
    Acc;
filters_to_query([{Key, Value}], _Separator, Acc) ->
    Acc ++ binary_to_list(Key) ++ " = '" ++ to_list(Value) ++ "'";
filters_to_query([{Key, Value} | T], Separator, Acc) ->
    NewAcc = Acc ++ binary_to_list(Key) ++ " = '" ++ to_list(Value) ++ Separator,
    filters_to_query([{Key, Value} | T], Separator, NewAcc).

to_list(List) when is_list(List) ->
    List;
to_list(Integer) when is_integer(Integer) ->
    integer_to_list(Integer);
to_list(Float) when is_float(Float) ->
    float_to_list(Float);
to_list(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
to_list(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
to_list(Tuple) when is_tuple(Tuple) ->
    lists:flatten(io_lib:format("~p", [Tuple]));
to_list(Map) when is_map(Map) ->
    lists:flatten(io_lib:format("~p", [Map]));
to_list(Other) ->
    lists:flatten(io_lib:format("~p", [Other])).
