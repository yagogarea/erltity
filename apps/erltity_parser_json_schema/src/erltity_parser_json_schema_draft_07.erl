-module(erltity_parser_json_schema_draft_07).

%%% EXTERNAL EXPORTS
-export([
    parse/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(Json) -> Schema when
    Json :: map(),
    Schema :: map().
parse(null) ->
    undefined;
parse(true) ->
    true;
parse(false) ->
    false;
parse(#{<<"enum">> := Enum, <<"type">> := Type}) ->
    #{type => binary_to_atom(Type), enum => Enum};
parse(#{<<"const">> := Const}) ->
    #{const => Const};
parse(#{<<"type">> := <<"null">>}) ->
    #{type => undefined};
parse(#{<<"type">> := <<"integer">>} = Schema) ->
    parse_integer(Schema);
parse(#{<<"type">> := <<"number">>} = Schema) ->
    parse_number(Schema);
parse(#{<<"type">> := <<"string">>} = Schema) ->
    parse_string(Schema);
parse(#{<<"type">> := <<"array">>} = Schema) ->
    parse_array(Schema);
parse(#{<<"type">> := <<"boolean">>}) ->
    #{type => boolean};
parse(#{<<"type">> := <<"object">>} = Schema) ->
    parse_object(Schema);
parse(#{<<"type">> := List}) when is_list(List) ->
    #{type => lists:map(fun binary_to_atom/1, List)};
parse(#{<<"not">> := Schema}) ->
    #{'not' => parse(Schema)};
parse(#{<<"oneOf">> := Schemas}) ->
    #{one_of => lists:map(fun parse/1, Schemas)};
parse(#{<<"anyOf">> := Schemas}) ->
    #{any_of => lists:map(fun parse/1, Schemas)};
parse(#{<<"allOf">> := Schemas}) ->
    #{all_of => lists:map(fun parse/1, Schemas)};
parse(_Schema) ->
    undefined_type.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
parse_object(Schema) ->
    PropertiesKeys = [
        max_properties, 
        min_properties,
        required,
        pattern_properties,
        additional_properties,
        dependencies,
        property_names
    ],
    NewSchema = build_schema(object, PropertiesKeys, Schema),
    parse_keywords_for_applying_subschemas(parse_attributes(properties, NewSchema, <<"properties">>, Schema), Schema).

parse_attributes(NewKey, NewMap, OldKey, OldMap) ->
    case maps:get(OldKey, OldMap, undefined) of
       undefined ->
            NewMap;
        Attributes when is_map(Attributes)->
            NewValue = maps:from_list(
                lists:foldl(
                    fun({Key1, Value}, Acc) ->
                        [{Key1, parse(Value)} | Acc]
                    end,
                    [],
                    maps:to_list(Attributes)
                )
            ),
            maps:put(NewKey, NewValue, NewMap);
        Attributes when is_list(Attributes) ->
            maps:put(NewKey, Attributes, NewMap)
    end.

parse_integer(Schema) ->
    IntegerKeys = [minimum, maximum, exclusive_minimum, exclusive_maximum, multiple_of],
    build_schema(integer, IntegerKeys, Schema).

parse_number(Schema) ->
    NumberKeys = [minimum, maximum, exclusive_minimum, exclusive_maximum],
    NewSchema = build_schema(NumberKeys, Schema),
    IntegerSchema = put_only_if_exists(multiple_of, NewSchema, <<"multipleOf">>, Schema),
    #{
        any_of => [
            maps:put(type, integer, IntegerSchema),
            maps:put(type, float, NewSchema)
        ]
    }.

parse_string(Schema) ->
    IntegerKeys = [min_length, max_length, pattern, format, content_encoding, content_media_type],
    build_schema(string, IntegerKeys, Schema).

parse_array(Schema) ->
    ArrayKeys = [additional_items, max_items, min_items, unique_items, contains],
    NewSchema = build_schema(array, ArrayKeys, Schema),
    case maps:get(<<"items">>, Schema, undefined) of
        undefined ->
            NewSchema;
        #{<<"type">> := Type} ->
            maps:put(items, #{type => binary_to_atom(Type)}, NewSchema);
        Items when is_list(Items)->
            maps:put(items, lists:map(fun parse/1, Items), NewSchema)
    end.

build_schema(Keys, Schema) ->
    lists:foldl(
        fun(Key, Acc) ->
            BinaryKey = list_to_binary(snake_case_to_camel_case(atom_to_list(Key))),
            put_only_if_exists(Key, Acc, BinaryKey, Schema)
        end,
        #{},
        Keys
    ).

build_schema(Type, Keys, Schema) ->
    SchemaAttr = build_schema(Keys, Schema),
    maps:put(type, Type, SchemaAttr).

parse_subschema(Keys, Schema) ->
    lists:foldl(
        fun(Key, Acc) ->
            BinaryKey = list_to_atom(camel_case_to_snake_case(binary_to_list(Key))),
            parse_attributes(BinaryKey, Acc, Key, Schema)
        end,
        #{},
        Keys
    ).

parse_subschemas(NewKey, NewMap, OldKey, OldMap) ->
    case maps:get(OldKey, OldMap, undefined) of
        undefined ->
            NewMap;
        List when is_list(List)->
            SubSchemas = lists:map(
                fun(Map) -> 
                    parse_subschema(maps:keys(Map), Map)
                end,
                List
            ),
            maps:put(NewKey, SubSchemas, NewMap);
        Map when is_map(Map) ->
            SubSchema = parse_subschema(maps:keys(Map), Map),
            maps:put(NewKey, SubSchema, NewMap)
    end.

parse_keywords_for_applying_subschemas(NewMap, Schema) ->
    Keywords = ['if', then, else, all_of, any_of, one_of, 'not'],
    lists:foldl(
        fun(Keyword, Acc) -> 
            parse_subschemas(Keyword, Acc, list_to_binary(snake_case_to_camel_case(atom_to_list(Keyword))), Schema)
        end,
        NewMap,
        Keywords
    ).

snake_case_to_camel_case(String) ->
    [First | Rest] = string:lexemes(String, "_"),
    string:join([First | lists:map(fun string:titlecase/1, Rest)], "").

camel_case_to_snake_case(String) ->
    lists:reverse(lists:foldl(
        fun(C, Acc) ->
            case C >= 65 andalso C =< 90 of
                true ->
                    [C + 32] ++ "_"  ++ Acc;
                false ->
                    [C] ++ Acc
            end 
        end,
        "",
        String
    )).

put_only_if_exists(NewKey, NewMap, OldKey, OldMap) ->
    case maps:get(OldKey, OldMap, undefined) of
        undefined ->
            NewMap;
        Value ->
            maps:put(NewKey, Value, NewMap)
    end.
