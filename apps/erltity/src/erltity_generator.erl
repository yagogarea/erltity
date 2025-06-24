-module(erltity_generator).

%%% EXTERNAL EXPORTS
-export([
    generate/3,
    generate_validate/3
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
generate(DbDriverModule, Schema, SchemaName) ->
    ModAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(SchemaName)]),
    ExportAttr = erltity_generator_utils:export([
        {validate, 1},
        {create, 1},
        {update, 2},
        {delete, 1},
        {find, 1},
        {find, 2}
    ]),
    BehaviourAttr = erl_syntax:attribute(
        erl_syntax:atom(behaviour), [
        erl_syntax:atom(erltity)
        ]
    ),
    ExternalExports = lists:map(
        fun({FunName, Args}) -> 
             erltity_generator_utils:db_call_function(
                    DbDriverModule,
                    FunName, 
                    SchemaName,
                    Args
                )
        end,
        [
            {create, ['CreateRequest']},
            {update, ['ID', 'UpdateRequest']},
            {delete, ['ID']},
            {find, ['ID']},
            {find, ['Filters', 'Opts']}
        ]
    ),
    {ValidateFunc, InternalExports, ValidateAuxFuns} = generate_validate(SchemaName, Schema, #{}),
    Attrs = 
        [
            ModAttr,
            BehaviourAttr,
            ExportAttr,
            InternalExports,
            ValidateFunc
        ]
        ++ ExternalExports
        ++ ValidateAuxFuns,
    [erl_syntax:revert(AST) || AST <- Attrs].

generate_validate(SchemaName, Schema, _Opts) ->
    {FunsName, ValidateExportsAcc, Functions} = add_requirements(SchemaName, maps:to_list(Schema)),
    {
        erl_syntax:function(
            erl_syntax:atom(validate), [
                erl_syntax:clause(
                    [erl_syntax:variable(erltity_generator_utils:atom_capitalize(SchemaName))],
                    none,
                    [
                        erl_syntax:application(
                            erl_syntax:atom(validate_all),
                            [
                                erl_syntax:list(lists:reverse(FunsName))
                            ]
                        )
                    ]
                )
            ]
        ),
        erltity_generator_utils:export(ValidateExportsAcc),
        lists:reverse([validate_one_function(), validate_all_function() | Functions])
    }.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
add_requirements(VariableName, [{type, integer} | T]) ->
    validate_number(VariableName, integer, T);
add_requirements(VariableName, [H | T]) when H =:= {anyOf,[#{type => integer}, #{type => float}]} ->
    validate_number(VariableName, number, T);
add_requirements(VariableName, [{type, array} | T]) ->
    validate_array(VariableName, T);
add_requirements(VariableName, [{type, string} | T]) ->
    validate_string(VariableName, T);
add_requirements(VariableName, [{type, boolean} | _T]) ->
    validate_type(VariableName, boolean);
add_requirements(VariableName, [{enum, List} | _T]) ->
    ParsedList = erl_syntax:list(lists:map(fun erltity_generator_utils:parse_element/1, List)),
    erltity_generator_utils:check_expression(
        erl_syntax:application(
            erl_syntax:atom(lists),
            erl_syntax:atom(member), 
            [
                erl_syntax:variable(VariableName),
                ParsedList
            ]
        ),
        invalid_enum_value,
        ParsedList,
        VariableName
    );
add_requirements(VariableName, [{type, List} | _T]) when is_list(List) ->
    ParsedList = erl_syntax:list(
        lists:map(
            fun(Type) ->
                validate_type(VariableName, Type)
            end,
            List
        )
    ),
    erltity_generator_utils:check_expression(
        erl_syntax:application(
            erl_syntax:atom(validate_one),
            [
                ParsedList
            ]
        ),
        invalid_type,
        ParsedList,
        VariableName
    );
add_requirements(VariableName, [{type, object} | T]) ->
    validate_object(VariableName, T).


validate_type(VariableName, Type) -> 
    erltity_generator_utils:wrap_in_zero_arity_fun(
        erltity_generator_utils:check_expression(
            erl_syntax:application(
                erl_syntax:atom(list_to_atom("is_" ++ atom_to_list(Type))), 
                [
                    erl_syntax:variable(VariableName)
                ]
            ),
            invalid_type,
            erl_syntax:atom(list_to_atom("expected_" ++ atom_to_list(Type))),
            VariableName
        )
    ).

validate_object(VariableName, Object) ->
    Properties = maps:to_list(proplists:get_value(properties, Object)),
    Required = proplists:get_value(required, Object, []),
    MapName = erltity_generator_utils:atom_capitalize(VariableName),
    ValidateKeysFun = erltity_generator_utils:wrap_in_zero_arity_fun(
        erl_syntax:application(
            erl_syntax:atom(validate_list),
            [
                erl_syntax:application(
                    erl_syntax:atom(maps), 
                    erl_syntax:atom(to_list),
                    [
                        erl_syntax:variable(MapName)
                    ]
                )
            ]
        )),
    case Required of
        [] ->
            {Exports, Functions} = validate_properties(Properties, {[], [validate_list_function(VariableName)]}),
            {[ValidateKeysFun], Exports, Functions};
        _Required ->
            Fun = erltity_generator_utils:wrap_in_zero_arity_fun(
                erl_syntax:application(
                    erl_syntax:atom(validate_requireds),
                    [
                        erl_syntax:variable(MapName),
                        erl_syntax:list(
                            lists:map(fun erltity_generator_utils:parse_element/1, Required)
                        )
                    ]
                )
            ),
            {Exports, Functions} = validate_properties(
                Properties, 
                {[],
                [
                    validate_list_function(VariableName),
                    validate_required_function(MapName)
                ]}
            ),
            {[ValidateKeysFun, Fun], Exports, Functions}
    end.

validate_properties([{PropertyName, Requirements}], {ExportsAcc, FunAcc}) ->
    FunName = list_to_atom("validate_" ++ binary_to_list(PropertyName)),
    Variable = binary_to_atom(erltity_generator_utils:capitalize(PropertyName)),
    Fun = generate_validate_property_function(FunName, Variable, Requirements),
    {[{FunName, 1} | ExportsAcc], [Fun | FunAcc]};
validate_properties([{PropertyName, Requirements} | T], {ExportsAcc, FunAcc}) ->
    FunName = list_to_atom("validate_" ++ binary_to_list(PropertyName)),
    Variable = binary_to_atom(erltity_generator_utils:capitalize(PropertyName)),
    Fun = generate_validate_property_function(FunName, Variable, Requirements),
    validate_properties(T, {[{FunName, 1} | ExportsAcc], [Fun | FunAcc]}).

generate_validate_property_function(FunName, Variable, Requirements) ->
    erl_syntax:function(
        erl_syntax:atom(FunName), [
            erl_syntax:clause(
                [erl_syntax:variable(Variable)],
                none,
                [
                    erl_syntax:application(
                            erl_syntax:atom(validate_all),
                            [
                                add_requirements(Variable, maps:to_list(Requirements))
                            ]
                        )
                ]
            )
        ]
    ).

%%%-----------------------------------------------------------------------------
%%% INTEGER VALIDATION FUNCTIONS
%%%-----------------------------------------------------------------------------
validate_number(VariableName, Type, Requirements) ->
    erl_syntax:list(
        validate_number_acc(
            VariableName,
            Requirements,
            [validate_type(VariableName, Type)]
        )
    ).

validate_number_acc(_VariableName, [], Acc) ->
    lists:reverse(Acc);
validate_number_acc(VariableName, [{minimum, Value} | T], Acc) ->
    validate_number_acc(VariableName, T, [
        erltity_generator_utils:wrap_in_zero_arity_fun(
            compare_numbers_function(VariableName, Value, '>=', value_below_minimun))| Acc
    ]);
validate_number_acc(VariableName, [{maximum, Value} | T], Acc) ->
    validate_number_acc(VariableName, T, [
        erltity_generator_utils:wrap_in_zero_arity_fun(compare_numbers_function(VariableName, Value, '=<', value_above_maximun)) 
        | Acc
    ]);
validate_number_acc(VariableName, [{exclusive_minimum, Value} | T], Acc) ->
    validate_number_acc(VariableName, T, [
        erltity_generator_utils:wrap_in_zero_arity_fun(
            compare_numbers_function(VariableName, Value, '>', value_below_exclusive_minimum))| Acc
    ]);
validate_number_acc(VariableName, [{exclusive_maximum, Value} | T], Acc) ->
    validate_number_acc(VariableName, T, [
        erltity_generator_utils:wrap_in_zero_arity_fun(compare_numbers_function(VariableName, Value, '<', value_above_exclusive_maximum)) 
        | Acc
    ]);
validate_number_acc(VariableName, [{multiple_of, Value} | T], Acc) ->
    validate_number_acc(VariableName, T, [
        erltity_generator_utils:wrap_in_zero_arity_fun(compare_numbers_function(VariableName, Value, 'rem', 0, not_multiple_of)) 
        | Acc
    ]).
%%%-----------------------------------------------------------------------------
%%% ARRAY VALIDATION FUNCTIONS
%%%-----------------------------------------------------------------------------
% [additional_items, max_items, min_items, unique_items, contains],
validate_array(VariableName, Requirements) ->
    erl_syntax:list(
        validate_array(
            VariableName,
            Requirements,
            [validate_type(VariableName, integer)]
        )
    ).

validate_array(_VariableName, [], Acc) ->
    lists:reverse(Acc);
validate_array(VariableName, [{items, Value} | T], Acc) ->
    validate_array(VariableName, T, [
        erltity_generator_utils:wrap_in_zero_arity_fun(
            compare_numbers_function(VariableName, Value, '>=', value_below_minimun))| Acc
    ]).

%%%-----------------------------------------------------------------------------
%%% STRING VALIDATION FUNCTIONS
%%%-----------------------------------------------------------------------------
validate_string(VariableName, Map) ->
    Requirements = proplists:delete(content_media_type, Map),
    case
        {
            proplists:get_value(format, Requirements, undefined),
            proplists:get_value(content_encoding, Requirements, undefined)
        }
    of
        {<<"binary">>, _AnyEncoding} ->
            erl_syntax:list(
                validate_binary(
                    VariableName,
                    proplists:delete(format, proplists:delete(content_encoding, Requirements)),
                    [validate_type(VariableName, binary)]
                )
            );
        {_AnyFormat, <<"base64">>} ->
            erl_syntax:list(
                validate_binary(
                    VariableName,
                    proplists:delete(format, proplists:delete(content_encoding, Requirements)),
                    [validate_type(VariableName, binary)]
                )
            );
        {<<"byte">>, _AnyEncoding} ->
            erl_syntax:list(
                validate_binary(
                    VariableName,
                    proplists:delete(format, proplists:delete(content_encoding, Requirements)),
                    [validate_type(VariableName, binary)]
                )
            );
        _ -> % TO DO: Validate other formats
            erl_syntax:list(
                validate_string(
                    VariableName,
                    proplists:delete(format, proplists:delete(content_encoding, Requirements)),
                    [validate_type(VariableName, list)]
                )
            )
    end.
validate_string(_VariableName, [], Acc) ->
    lists:reverse(Acc);
validate_string(VariableName, [{min_length, Value} | T], Acc) ->
    validate_string(VariableName, T, [
        erltity_generator_utils:wrap_in_zero_arity_fun(min_length_function(VariableName, Value))| Acc
    ]);
validate_string(VariableName, [{max_length, Value} | T], Acc) ->
    validate_string(VariableName, T, [
       erltity_generator_utils:wrap_in_zero_arity_fun(max_length_function(VariableName, Value)) | Acc
    ]);
validate_string(VariableName, [{pattern, Value} | T], Acc) ->
    validate_string(VariableName, T, [
       erltity_generator_utils:wrap_in_zero_arity_fun(pattern_function(VariableName, binary_to_list(Value))) | Acc
    ]).

%%%-----------------------------------------------------------------------------
%%% BINARY VALIDATION FUNCTIONS
%%%-----------------------------------------------------------------------------
validate_binary(_VariableName, [], Acc) ->
    lists:reverse(Acc);
validate_binary(VariableName, [{min_length, Value} | T], Acc) ->
    validate_binary(VariableName, T, [
        erltity_generator_utils:wrap_in_zero_arity_fun(binary_min_length_function(VariableName, Value))| Acc
    ]);
validate_binary(VariableName, [{max_length, Value} | T], Acc) ->
    validate_binary(VariableName, T, [
       erltity_generator_utils:wrap_in_zero_arity_fun(binary_max_length_function(VariableName, Value)) | Acc
    ]);
validate_binary(VariableName, [{pattern, Value} | T], Acc) ->
    validate_binary(VariableName, T, [
       erltity_generator_utils:wrap_in_zero_arity_fun(pattern_function(VariableName, binary_to_list(Value))) | Acc
    ]).

%%%-----------------------------------------------------------------------------
%%% GENERATED FUNCTIONS
%%%-----------------------------------------------------------------------------
compare_numbers_function(VariableName, Number, Operator, Error) ->
    erltity_generator_utils:check_expression(
        erl_syntax:infix_expr(
            erl_syntax:variable(VariableName),
            erl_syntax:operator(Operator),
            erl_syntax:integer(Number)
        ),
        Error,
        erl_syntax:integer(Number),
        VariableName
    ).

compare_numbers_function(VariableName, Number, Operator, Expected, Error) ->
    erltity_generator_utils:check_expression(
        erl_syntax:infix_expr(
            erl_syntax:infix_expr(
                erl_syntax:variable(VariableName),
                erl_syntax:operator(Operator),
                erl_syntax:integer(Number)
            ),
            erl_syntax:operator('=='),
            erl_syntax:integer(Expected)
        ),
        Error,
        erl_syntax:integer(Number),
        VariableName
    ).

binary_min_length_function(VariableName, MinLength) ->
    erltity_generator_utils:check_expression(
        erl_syntax:infix_expr(
            erl_syntax:application(
                erl_syntax:atom(byte_size),
                [erl_syntax:variable(VariableName)]
            ),
            erl_syntax:operator('>='),
            erl_syntax:integer(MinLength)
        ),
        length_below_minimum,
        erl_syntax:integer(MinLength),
        VariableName
    ).

binary_max_length_function(VariableName, MinLength) ->
    erltity_generator_utils:check_expression(
        erl_syntax:infix_expr(
            erl_syntax:application(
                erl_syntax:atom(byte_size),
                [erl_syntax:variable(VariableName)]
            ),
            erl_syntax:operator('=<'),
            erl_syntax:integer(MinLength)
        ),
        length_exceeds_maximum,
        erl_syntax:integer(MinLength),
        VariableName
    ).

min_length_function(VariableName, MinLength) ->
    erltity_generator_utils:check_expression(
        erl_syntax:infix_expr(
            erl_syntax:application(
                erl_syntax:atom(length),
                [erl_syntax:variable(VariableName)]
            ),
            erl_syntax:operator('>='),
            erl_syntax:integer(MinLength)
        ),
        length_below_minimum,
        erl_syntax:integer(MinLength),
        VariableName
    ).

max_length_function(VariableName, MinLength) ->
    erltity_generator_utils:check_expression(
        erl_syntax:infix_expr(
            erl_syntax:application(
                erl_syntax:atom(length),
                [erl_syntax:variable(VariableName)]
            ),
            erl_syntax:operator('=<'),
            erl_syntax:integer(MinLength)
        ),
        length_exceeds_maximum,
        erl_syntax:integer(MinLength),
        VariableName
    ).

pattern_function(VariableName, Pattern) ->
    erltity_generator_utils:check_expression(
            erl_syntax:application(
                erl_syntax:atom(re),
                erl_syntax:atom(run),
                [
                    erl_syntax:variable(VariableName),
                    erl_syntax:string(Pattern),
                    erl_syntax:list([
                        erl_syntax:tuple([
                            erl_syntax:atom(capture),
                            erl_syntax:atom(none)
                        ])
                    ])
                ]
            ),
        erl_syntax:atom(match),
        erl_syntax:atom(nomatch),
        pattern_mismatch,
        erl_syntax:string(Pattern),
        VariableName
    ).

validate_all_function() ->
    FunctionName = validate_all,
    Return = erl_syntax:atom(true),
    HeadName = erl_syntax:variable('F'),
    TailName = 'Rest',
    Condition = erl_syntax:application(
        erl_syntax:variable('F'),
        []
    ),
    Clause1 = erl_syntax:atom(true),
    Clause2 = erl_syntax:variable('Error'),
    FailReturn = erl_syntax:variable('Error'),

    erltity_generator_utils:tail_recursion_function(
        FunctionName,
        Return,
        HeadName,
        TailName,
        [],
        Condition,
        Clause1,
        Clause2,
        FailReturn
    ).

validate_one_function() ->
    FunctionName = validate_one,
    Return = erl_syntax:atom(true),
    HeadName = 'F',
    TailName = 'Rest',
    Condition = erl_syntax:application(
        erl_syntax:variable('F'),
        []
    ),
    erltity_generator_utils:tail_recursion_function(
        validate_one,
        [],
        Return,
        HeadName,
        TailName,
        [],
        Condition,
        [
            {erl_syntax:atom(true), erl_syntax:atom(true)},
            {
                erl_syntax:underscore(), 
                erl_syntax:application(
                    erl_syntax:atom(FunctionName),
                    [erl_syntax:variable(TailName)])
            }
        ]
    ).

validate_required_function(VariableName) ->
    Args = [VariableName],
    Return = erl_syntax:atom(true),
    Condition =
        erl_syntax:application(
            erl_syntax:atom(maps),
            erl_syntax:atom(is_key),
            [
                erl_syntax:variable('Key'),
                erl_syntax:variable(VariableName)
            ]
        ),
    FailReturn =
        erl_syntax:tuple([
            erl_syntax:atom(missing_attribute),
            erl_syntax:variable('Key'),
            erl_syntax:variable(VariableName)
        ]),
    Clause1 = erl_syntax:atom(true),
    Clause2 = erl_syntax:atom(false),
    erltity_generator_utils:tail_recursion_function(
        validate_requireds,
        Args,
        Return,
        'Key',
        'Rest',
        [],
        Condition,
        Clause1,
        Clause2,
        FailReturn
    ).

validate_list_function(EntityName) ->
    FunctionName = validate_list,
    Return = erl_syntax:atom(true),
    TailName = 'T',

    Head = erl_syntax:tuple([
        erl_syntax:variable('Key'),
        erl_syntax:variable('Value')
    ]),

    PrevFunction = [erl_syntax:match_expr(
        erl_syntax:variable('ValidateFunction'),
        erl_syntax:application(
            erl_syntax:atom(binary_to_atom),
            [
                erl_syntax:binary([
                    erl_syntax:binary_field(erl_syntax:string("validate_")),
                    erl_syntax:binary_field(erl_syntax:variable('Key'), [erl_syntax:atom(binary)])
                ])
            ]
        )
    )],

    Condition = 
        erl_syntax:application(
            erl_syntax:atom(EntityName),
            erl_syntax:variable('ValidateFunction'),
            [
                erl_syntax:variable('Value')
            ]
        ),

    Clause1 = erl_syntax:atom(true),
    Clause2 = erl_syntax:variable('Error'),
    FailReturn = erl_syntax:variable('Error'),

    erltity_generator_utils:tail_recursion_function(
        FunctionName,
        Return,
        Head,
        TailName,
        PrevFunction,
        Condition,
        Clause1,
        Clause2,
        FailReturn
    ).
