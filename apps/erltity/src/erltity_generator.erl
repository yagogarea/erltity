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
    {ValidatesFuns, ValidateExports, ValidateAuxFuns} = generate_validate(SchemaName, Schema, #{}),
    Attrs = 
        [
            ModAttr,
            BehaviourAttr,
            ExportAttr,
            erltity_generator_utils:export(ValidateExports)
        ]
        ++ ExternalExports
        ++ ValidatesFuns
        ++ ValidateAuxFuns,
    [erl_syntax:revert(AST) || AST <- Attrs].

generate_validate(SchemaName, Schema, _Opts) ->
        add_requirements(
            validate,
            SchemaName,
            Schema,
            ordsets:new(),
            ordsets:new()
        ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
add_requirements(FunName, VariableName, _Schema = #{enum := List}, Exports, Functions) ->
    ParsedList = erl_syntax:list(lists:map(fun erltity_generator_utils:parse_element/1, List)),
    Fun = erl_syntax:function(
        erl_syntax:atom(FunName), [
            erl_syntax:clause(
                [erl_syntax:variable(erltity_generator_utils:atom_capitalize(VariableName))],
                none,
                [
                    erl_syntax:application(
                        erl_syntax:atom(validate_enum),
                        [
                            erl_syntax:variable(erltity_generator_utils:atom_capitalize(VariableName)),
                            ParsedList
                        ]
                    )
                ]
            )
        ]
    ),
    EnumFun = erl_syntax:function(
        erl_syntax:atom(validate_enum),
        [
            erl_syntax:clause(
                [erl_syntax:variable('Value'), erl_syntax:variable('EnumList')],
                none,
                [
                    erltity_generator_utils:check_expression(
                        erl_syntax:application(
                            erl_syntax:atom(lists),
                            erl_syntax:atom(member),
                            [
                                erl_syntax:variable('Value'),
                                erl_syntax:variable('EnumList')
                            ]
                        ),
                        invalid_enum_value,
                        erl_syntax:variable('EnumList'),
                        'Value'
                    )
                ]
            )
        ]
    ),
    NewExports = ordsets:add_element({FunName, 1}, Exports),
    {Fun, NewExports, ordsets:add_element(EnumFun, Functions)};
add_requirements(FunName, VariableName, Schema = #{type := integer}, Exports, Functions) ->
    Requirements = maps:to_list(maps:remove(type, Schema)),
    validate_number(FunName, VariableName, integer, Requirements, Exports, Functions);
% add_requirements(FunName, VariableName, Schema = #{any_of := [#{type := integer}, #{type := float}]}, Exports, Functions) ->
%     validate_number(FunName, VariableName, number, maps:to_list(maps:remove(anyOf, Schema)), Exports, Functions);
add_requirements(FunName, VariableName, Schema = #{type := array}, Exports, Functions) ->
    Requirements = maps:to_list(maps:remove(type, Schema)),
    validate_array(FunName, VariableName, Requirements, Exports, Functions);
add_requirements(FunName, VariableName, Schema = #{type := string}, Exports, Functions) ->
    Requirements = maps:to_list(maps:remove(type, Schema)),
    validate_string(FunName, VariableName, Requirements, Exports, Functions);
add_requirements(FunName, VariableName, _Schema = #{type := boolean}, Exports, Functions) ->
    FunAux = validate_type(boolean),
    Fun = erl_syntax:function(
        erl_syntax:atom(FunName), [
            erl_syntax:clause(
                [erl_syntax:variable(erltity_generator_utils:atom_capitalize(VariableName))],
                none,
                [
                     erl_syntax:application(
                        erl_syntax:atom(validate_type_boolean),
                        [erl_syntax:variable(erltity_generator_utils:atom_capitalize(VariableName))]
                    )
                ]
            )
        ]
    ),
    NewExports = ordsets:add_element({FunName, 1}, Exports),
    {Fun, NewExports, ordsets:add_element(FunAux, Functions) };

add_requirements(FunName, VariableName, Schema = #{type := object}, Exports, Functions) ->
    validate_object(FunName, VariableName, maps:remove(type, Schema), Exports, Functions).

validate_type(string) -> 
    validate_type(list);
validate_type(Type) -> 
    VariableName = erltity_generator_utils:atom_capitalize(Type),
    erl_syntax:function(
            erl_syntax:atom(list_to_atom("validate_type_" ++ atom_to_list(Type))),
            [
                erl_syntax:clause(
                    [erl_syntax:variable(VariableName)],
                    [],
                    [
                        erltity_generator_utils:check_expression(
                            erl_syntax:application(
                                erl_syntax:atom(list_to_atom("is_" ++ atom_to_list(Type))),
                                [erl_syntax:variable(VariableName)]
                            ),
                            invalid_type,
                            erl_syntax:atom(list_to_atom("expected_" ++ atom_to_list(Type))),
                            VariableName
                        )
                    ]
                )
            ]
        ).

validate_object(FunName, VariableName, Object, Exports, Functions) ->
    Properties = maps:to_list(maps:get(properties, Object)),
    Required = maps:get(required, Object, []),
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
            {NewFun, NewExports, NewFunctions} = 
                validate_properties(
                    Properties,
                    {
                        [],
                        Exports,
                        ordsets:add_element(validate_list_function(VariableName), Functions)
                    }
                ),
            ValidateCalls = [ValidateKeysFun];
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
            {NewFun, NewExports, NewFunctions} = validate_properties(
                Properties, 
                {
                    [],
                    Exports,
                    ordsets:add_element(
                        validate_list_function(VariableName),
                        ordsets:add_element(validate_required_function('Map'), Functions)
                    )
                }
            ),
            ValidateCalls = [ValidateKeysFun, Fun]
    end,
    Validate = erl_syntax:function(
        erl_syntax:atom(FunName), [
            erl_syntax:clause(
                [erl_syntax:variable(erltity_generator_utils:atom_capitalize(VariableName))],
                none,
                [
                    erl_syntax:application(
                        erl_syntax:atom(validate_all),
                        [
                            erl_syntax:list(lists:reverse(ValidateCalls))
                        ]
                    )
                ]
            )
        ]
    ),
    {[Validate | lists:reverse(NewFun)], NewExports, ordsets:add_element(validate_all_function(), NewFunctions)}.

validate_properties([{PropertyName, Requirements}], {PropertiesFunAcc, ExportsAcc, FunAcc}) ->
    FunName = list_to_atom("validate_" ++ binary_to_list(PropertyName)),
    Variable = binary_to_atom(erltity_generator_utils:capitalize(PropertyName)),
    {NewFun, NewExports, NewFunAcc} = add_requirements(FunName, Variable, Requirements, ExportsAcc, FunAcc),
    {[NewFun | PropertiesFunAcc], NewExports, NewFunAcc};
validate_properties([{PropertyName, Requirements} | T], {PropertiesFunAcc, ExportsAcc, FunAcc}) ->
    FunName = list_to_atom("validate_" ++ binary_to_list(PropertyName)),
    Variable = binary_to_atom(erltity_generator_utils:capitalize(PropertyName)),
    {NewFun, NewExports, NewFunAcc} = add_requirements(FunName, Variable, Requirements, ExportsAcc, FunAcc),
    validate_properties(
        T,
        {
            [NewFun | PropertiesFunAcc],
            NewExports,
            NewFunAcc
        }
    ).

generate_validate_property_function(FunName, Variable, FunCalls) ->
    erl_syntax:function(
        erl_syntax:atom(FunName), [
            erl_syntax:clause(
                [erl_syntax:variable(Variable)],
                none,
                [
                    erl_syntax:application(
                        erl_syntax:atom(validate_all),
                        [
                            erl_syntax:list(FunCalls)
                        ]
                    )
                ]
            )
        ]
    ).

%%%-----------------------------------------------------------------------------
%%% INTEGER VALIDATION FUNCTIONS
%%%-----------------------------------------------------------------------------
validate_number(FunName, VariableName, Type, Requirements, Exports, Functions) ->
    FunCall = fun_call_validate_type(Type, VariableName),
    {FunCalls, NewExports, NewFuns} = validate_number_acc(
        FunName,
        VariableName,
        Requirements,
        [FunCall],
        Exports,
        ordsets:add_element(validate_type(Type), Functions)
    ),
    NewFun = generate_validate_property_function(FunName, VariableName, lists:reverse(FunCalls)),
    {NewFun, ordsets:add_element({FunName, 1}, NewExports), NewFuns}.

validate_number_acc(_FunName, _VariableName, [], FunCalls, Exports, Functions) ->
    {FunCalls, Exports, Functions};
validate_number_acc(_FunName, VariableName, [{minimum, Value} | T], FunCalls, Exports, Functions) ->
    {FunCall, Fun} = compare_numbers_function(validate_minimum, VariableName, 'Minimun', Value, '>=', value_below_minimum),
    validate_number_acc(
        _FunName,
        VariableName,
        T, 
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_number_acc(_FunName, VariableName, [{maximum, Value} | T], FunCalls, Exports, Functions) ->
    {FunCall, Fun} = compare_numbers_function(validate_maximum, VariableName, 'Maximum', Value, '=<', value_above_maximum),
    validate_number_acc(
        _FunName,
        VariableName,
        T, 
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_number_acc(_FunName, VariableName, [{exclusive_minimum, Value} | T], FunCalls, Exports, Functions) ->
     {FunCall, Fun} = compare_numbers_function(validate_exclusive_minimum, VariableName, 'ExclusiveMinimum', Value, '>', value_below_exclusive_minimum),
        validate_number_acc(
        _FunName,
        VariableName,
        T, 
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_number_acc(_FunName, VariableName, [{exclusive_maximum, Value} | T], FunCalls, Exports, Functions) ->
     {FunCall, Fun} = compare_numbers_function(validate_exclusive_maximum, VariableName, 'ExclusiveMaximum', Value, '>', value_above_exclusive_maximum),
        validate_number_acc(
        _FunName,
        VariableName,
        T, 
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_number_acc(_FunName, VariableName, [{multiple_of, Value} | T], FunCalls, Exports, Functions) ->
    {FunCall, Fun} = compare_numbers_function(validate_multiple_of, VariableName, 'MultipleOf', Value, 'rem', not_multiple_of),
    validate_number_acc(
        _FunName,
        VariableName,
        T, 
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    ).
%%%-----------------------------------------------------------------------------
%%% ARRAY VALIDATION FUNCTIONS
%%%-----------------------------------------------------------------------------
% TO DO: Validate contains property
validate_array(FunName, VariableName, T, Exports, Functions) ->
    validate_array(FunName, VariableName, T, [], Exports, Functions).

validate_array(FunName, VariableName, [], FunCalls, Exports, Functions) ->
    NewFun = generate_validate_property_function(FunName, VariableName, lists:reverse(FunCalls)),
    {NewFun, ordsets:add_element({FunName, 1}, Exports), Functions};
validate_array(FunName, VariableName, [{min_items, Value} | T], FunCalls, Exports, Functions) ->
    FunCall = erltity_generator_utils:fun_call_in_zero_arity_fun(
        validate_min_items,
        [
            erl_syntax:variable(VariableName),
            erl_syntax:integer(Value)
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(validate_min_items),
        [
            erl_syntax:clause(
                [
                    erl_syntax:variable('Value'),
                    erl_syntax:variable('MinItems')
                ],
                none,
                [min_length_function('Value', 'MinItems')]
            )
        ]
    ),
    validate_array(
        FunName,
        VariableName,
        T,
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_array(FunName, VariableName, [{max_items, Value} | T], FunCalls, Exports, Functions) ->
    FunCall = erltity_generator_utils:fun_call_in_zero_arity_fun(
        validate_max_items,
        [
            erl_syntax:variable(VariableName),
            erl_syntax:integer(Value)
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(validate_max_items),
        [
            erl_syntax:clause(
                [
                    erl_syntax:variable('Value'),
                    erl_syntax:variable('MaxItems')
                ],
                none,
                [max_length_function('Value', 'MaxItems')]
            )
        ]
    ),
    validate_array(
        FunName,
        VariableName,
        T,
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_array(FunName, VariableName, [{unique_items, true} | T], FunCalls, Exports, Functions) ->
    FunCall = erltity_generator_utils:fun_call_in_zero_arity_fun(
        validate_unique_items,
        [
            erl_syntax:variable(VariableName)
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(validate_unique_items),
        [
            erl_syntax:clause(
                [
                    erl_syntax:variable('Value')
                ],
                none,
                [erltity_generator_utils:check_expression(
                    erl_syntax:infix_expr(
                        erl_syntax:application(
                            erl_syntax:atom(length),
                            [erl_syntax:variable('Value')]
                        ),
                        erl_syntax:operator('=='),
                        erl_syntax:application(
                            erl_syntax:atom(length),
                            [
                                erl_syntax:application(
                                    erl_syntax:atom(lists),
                                    erl_syntax:atom(usort),
                                    [erl_syntax:variable('Value')]
                                )
                            ]
                        )
                    ),
                    not_unique_items,
                    erl_syntax:variable('Value'),
                    'Value'
                )]
            )
        ]
    ),
    validate_array(
        FunName,
        VariableName,
        T,
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_array(FunName, VariableName, Proplist, FunCalls, Exports, Functions) ->
    ItemSchema = proplists:get_value(items, Proplist),
    AdditionalItems = proplists:get_value(additional_items, Proplist, true),
    {FunCallsWithLength, NewFunctions1} =
        case {is_list(ItemSchema), AdditionalItems} of
            {true, false} ->
                LengthCheckFunCall = erltity_generator_utils:fun_call_in_zero_arity_fun(
                    validate_exact_length,
                    [
                        erl_syntax:variable(VariableName),
                        erl_syntax:integer(length(ItemSchema))
                    ]
                ),
                LengthCheckFun =
                    erl_syntax:function(
                        erl_syntax:atom(validate_exact_length),
                        [
                            erl_syntax:clause(
                                [
                                    erl_syntax:variable('Value'),
                                    erl_syntax:variable('ExpectedLength')
                                ],
                                none,
                                [
                                    erltity_generator_utils:check_expression(
                                        erl_syntax:infix_expr(
                                            erl_syntax:application(
                                                erl_syntax:atom(length),
                                                [erl_syntax:variable('Value')]
                                            ),
                                            erl_syntax:operator('=='),
                                            erl_syntax:variable('ExpectedLength')
                                        ),
                                        array_length_mismatch,
                                        erl_syntax:variable('ExpectedLength'),
                                        'Value'
                                    )
                                ]
                            )
                        ]
                    ),
                { [LengthCheckFunCall], ordsets:add_element(LengthCheckFun, Functions) };
            _ ->
                {[], Functions}
        end,
    {FunCallsTypes, NewFunctions2} =
        case ItemSchema of
            List when is_list(List) ->
                FunList = [fun_call_validate_type(maps:get(type, Type), VariableName) || Type <- List],
                FunListSyntax = erl_syntax:list(FunList),
                FunCallsForList = erltity_generator_utils:fun_call_in_zero_arity_fun(
                    validate_in_order,
                    [
                        FunListSyntax,
                        erl_syntax:variable(VariableName)
                    ]
                ),
                NewFuncs = lists:foldl(
                    fun(Type2, Acc) ->
                        ordsets:add_element(validate_type(maps:get(type, Type2)), Acc)
                    end,
                    NewFunctions1,
                    List
                ),
                {[FunCallsForList], ordsets:add_element(validate_in_order_function(), NewFuncs)};
            #{type := Type} ->
                {
                    [fun_call_validate_type(Type, VariableName)],
                    ordsets:add_element(validate_type(Type), NewFunctions1)
                }
        end,
    NewFunCalls =  FunCallsTypes ++ FunCallsWithLength ++ FunCalls,
    validate_array(
        FunName,
        VariableName,
        proplists:delete(items, proplists:delete(additional_items, Proplist)),
        NewFunCalls,
        Exports,
        NewFunctions2
    ).

%%%-----------------------------------------------------------------------------
%%% STRING VALIDATION FUNCTIONS
%%%-----------------------------------------------------------------------------
validate_string(FunName, VariableName, T, Exports, Functions) ->
    Requirements = proplists:delete(content_media_type, T),
    case
        {
            proplists:get_value(format, Requirements, undefined),
            proplists:get_value(content_encoding, Requirements, undefined)
        }
    of
        {<<"binary">>, _AnyEncoding} ->
            validate_binary(
                FunName,
                VariableName,
                proplists:delete(content_media_type, proplists:delete(format, proplists:delete(content_encoding, Requirements))),
                [fun_call_validate_type(binary, VariableName)],
                Exports,
                ordsets:add_element(validate_type(binary), Functions)
            );
        {_AnyFormat, <<"base64">>} ->
            validate_binary(
                FunName,
                VariableName,
               proplists:delete(content_media_type, proplists:delete(format, proplists:delete(content_encoding, Requirements))),
                [fun_call_validate_type(binary, VariableName)],
                Exports,
                ordsets:add_element(validate_type(binary), Functions)
            );
        {<<"byte">>, _AnyEncoding} ->
            validate_binary(
                FunName,
                VariableName,
                proplists:delete(content_media_type, proplists:delete(format, proplists:delete(content_encoding, Requirements))),
                [fun_call_validate_type(binary, VariableName)],
                Exports,
                ordsets:add_element(validate_type(binary), Functions)
            );
        _ -> % TO DO: Validate other formats
            validate_string(
                FunName,
                VariableName,
                proplists:delete(format, proplists:delete(content_encoding, Requirements)),
                [fun_call_validate_type(list, VariableName)],
                Exports,
                ordsets:add_element(validate_type(list), Functions)
            )
    end.
validate_string(FunName, VariableName, [], FunCalls, Exports, Functions) ->
    NewFun = generate_validate_property_function(FunName, VariableName, lists:reverse(FunCalls)),
    {NewFun, ordsets:add_element({FunName, 1}, Exports), Functions};
validate_string(FunName, VariableName, [{min_length, Value} | T], FunCalls, Exports, Functions) ->
    FunCall =  erltity_generator_utils:fun_call_in_zero_arity_fun(
        validate_min_length,
        [
            erl_syntax:variable(VariableName),
            erl_syntax:integer(Value)
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(validate_min_length),
        [
            erl_syntax:clause(
                [
                    erl_syntax:variable('Value'),
                    erl_syntax:variable('MinLength')
                ],
                none,
                [min_length_function('Value', 'MinLength')]
            )
        ]
    ),
    validate_string(
        FunName,
        VariableName,
        T,
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_string(FunName, VariableName, [{max_length, Value} | T], FunCalls, Exports, Functions) ->
    FunCall =  erltity_generator_utils:fun_call_in_zero_arity_fun(
        validate_max_length,
        [
            erl_syntax:variable(VariableName),
            erl_syntax:integer(Value)
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(validate_max_length),
        [
            erl_syntax:clause(
                [
                    erl_syntax:variable('Value'),
                    erl_syntax:variable('MaxLength')
                ],
                none,
                [max_length_function('Value', 'MaxLength')]
            )
        ]
    ),
    validate_string(
        FunName,
        VariableName,
        T,
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_string(FunName, VariableName, [{pattern, Value} | T], FunCalls, Exports, Functions) ->
    FunCall = erltity_generator_utils:fun_call_in_zero_arity_fun(
        validate_pattern,
        [
            erl_syntax:variable(VariableName),
            erl_syntax:string(binary_to_list(Value))
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(validate_pattern),
        [
            erl_syntax:clause(
                [
                    erl_syntax:variable('Value'),
                    erl_syntax:variable('Pattern')
                ],
                none,
                [pattern_function('Value', 'Pattern')]
            )
        ]
    ),
    validate_string(
        FunName,
        VariableName,
        T,
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    ).

%%%-----------------------------------------------------------------------------
%%% BINARY VALIDATION FUNCTIONS
%%%-----------------------------------------------------------------------------
validate_binary(FunName, VariableName, [], FunCalls, Exports, Functions) ->
    NewFun = generate_validate_property_function(FunName, VariableName, lists:reverse(FunCalls)),
    {NewFun, ordsets:add_element({FunName, 1}, Exports), Functions};
validate_binary(FunName, VariableName, [{min_length, Value} | T], FunCalls, Exports, Functions) ->
    FunCall = erltity_generator_utils:fun_call_in_zero_arity_fun(
        validate_binary_min_length,
        [
            erl_syntax:variable(VariableName),
            erl_syntax:integer(Value)
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(validate_binary_min_length),
        [
            erl_syntax:clause(
                [
                    erl_syntax:variable('Value'),
                    erl_syntax:variable('MinLength')
                ],
                none,
                [binary_min_length_function('Value', 'MinLength')]
            )
        ]
    ),
    validate_binary(
        FunName,
        VariableName,
        T,
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_binary(FunName, VariableName, [{max_length, Value} | T], FunCalls, Exports, Functions) ->
    FunCall = erltity_generator_utils:fun_call_in_zero_arity_fun(
        validate_binary_max_length,
        [
            erl_syntax:variable(VariableName),
            erl_syntax:integer(Value)
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(validate_binary_max_length),
        [
            erl_syntax:clause(
                [
                    erl_syntax:variable('Value'),
                    erl_syntax:variable('MaxLength')
                ],
                none,
                [binary_max_length_function('Value', 'MaxLength')]
            )
        ]
    ),
    validate_binary(
        FunName,
        VariableName,
        T,
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    );
validate_binary(FunName, VariableName, [{pattern, Value} | T], FunCalls, Exports, Functions) ->
    FunCall = erltity_generator_utils:fun_call_in_zero_arity_fun(
        validate_binary_pattern,
        [
            erl_syntax:variable(VariableName),
            erl_syntax:string(binary_to_list(Value))
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(validate_binary_pattern),
        [
            erl_syntax:clause(
                [
                    erl_syntax:variable('Value'),
                    erl_syntax:variable('Pattern')
                ],
                none,
                [pattern_function('Value', 'Pattern')]
            )
        ]
    ),
    validate_binary(
        FunName,
        VariableName,
        T,
        [FunCall | FunCalls],
        Exports,
        ordsets:add_element(Fun, Functions)
    ).

%%%-----------------------------------------------------------------------------
%%% GENERATED FUNCTIONS
%%%-----------------------------------------------------------------------------
fun_call_validate_type(string, VariableName) ->
    fun_call_validate_type(list, VariableName);
fun_call_validate_type(Type, VariableName) ->
    erltity_generator_utils:fun_call_in_zero_arity_fun(
        list_to_atom("validate_type_" ++ atom_to_list(Type)),
        [erl_syntax:variable(VariableName)]
    ).

compare_numbers_function(FunName, VariableName, LimitName, Number, Operator, Error) ->
    CheckExpr =
        case Operator of
            'rem' ->
                Expr1 = erl_syntax:infix_expr(
                    erl_syntax:infix_expr(
                        erl_syntax:variable('Number'),
                        erl_syntax:operator('rem'),
                        erl_syntax:variable(LimitName)
                    ),
                    erl_syntax:operator('=='),
                    erl_syntax:integer(0)
                ),
                    erltity_generator_utils:check_expression(
                        Expr1,
                        Error,
                        erl_syntax:integer(Number),
                        'Number'
                    );
            _ ->
                Expr2 = erl_syntax:infix_expr(
                    erl_syntax:variable('Number'),
                    erl_syntax:operator(Operator),
                    erl_syntax:variable(LimitName)
                ),
                    erltity_generator_utils:check_expression(
                        Expr2,
                        Error,
                        erl_syntax:variable(LimitName),
                        'Number'
                    )
        end,

    {
        erltity_generator_utils:wrap_in_zero_arity_fun(
            erl_syntax:application(
                erl_syntax:atom(FunName),
                [
                    erl_syntax:variable(VariableName),
                    erl_syntax:integer(Number)
                ]
            )
        ),
        erl_syntax:function(
            erl_syntax:atom(FunName),
            [
                erl_syntax:clause(
                    [
                        erl_syntax:variable('Number'),
                        erl_syntax:variable(LimitName)
                    ],
                    none,
                    [CheckExpr]
                )
            ]
        )
    }.

binary_min_length_function(VariableName, MinLength) ->
    erltity_generator_utils:check_expression(
        erl_syntax:infix_expr(
            erl_syntax:application(
                erl_syntax:atom(byte_size),
                [erl_syntax:variable(VariableName)]
            ),
            erl_syntax:operator('>='),
            erl_syntax:variable(MinLength)
        ),
        length_below_minimum,
        erl_syntax:variable(MinLength),
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
            erl_syntax:variable(MinLength)
        ),
        length_exceeds_maximum,
        erl_syntax:variable(MinLength),
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
            erl_syntax:variable(MinLength)
        ),
        length_below_minimum,
        erl_syntax:variable(MinLength),
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
            erl_syntax:variable(MinLength)
        ),
        length_exceeds_maximum,
        erl_syntax:variable(MinLength),
        VariableName
    ).

pattern_function(VariableName, Pattern) ->
    erltity_generator_utils:check_expression(
            erl_syntax:application(
                erl_syntax:atom(re),
                erl_syntax:atom(run),
                [
                    erl_syntax:variable(VariableName),
                    erl_syntax:variable(Pattern),
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
        erl_syntax:variable(Pattern),
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

validate_in_order_function() ->
    FunctionName = validate_in_order,
    Return = erl_syntax:atom(true),
    HeadF = 'F',
    TailFs = 'RestFuns',
    HeadE = 'Element',
    TailEs = 'Rest',
    Clause1 = erl_syntax:clause(
        [erl_syntax:nil(), erl_syntax:underscore()],
        [],
        [Return]
    ),
    ParamFs = erl_syntax:cons(
        erl_syntax:variable(HeadF),
        erl_syntax:variable(TailFs)
    ),
    ParamEs = erl_syntax:cons(
        erl_syntax:variable(HeadE),
        erl_syntax:variable(TailEs)
    ),
    Condition = erl_syntax:application(
        erl_syntax:variable(HeadF),
        [erl_syntax:variable(HeadE)]
    ),
    CaseExpr = erl_syntax:case_expr(
        Condition,
        [
            erl_syntax:clause(
                [erl_syntax:atom(true)],
                [],
                [
                    erl_syntax:application(
                        erl_syntax:atom(FunctionName),
                        [erl_syntax:variable(TailFs), erl_syntax:variable(TailEs)]
                    )
                ]
            ),
            erl_syntax:clause(
                [erl_syntax:variable('Error')],
                [],
                [erl_syntax:variable('Error')]
            )
        ]
    ),

    Clause2 = erl_syntax:clause(
        [ParamFs, ParamEs],
        [],
        [CaseExpr]
    ),

    erl_syntax:function(
        erl_syntax:atom(FunctionName),
        [Clause1, Clause2]
    ).


% validate_one_function() ->
%     FunctionName = validate_one,
%     Return = erl_syntax:atom(true),
%     HeadName = 'F',
%     TailName = 'Rest',
%     Condition = erl_syntax:application(
%         erl_syntax:variable('F'),
%         []
%     ),
%     erltity_generator_utils:tail_recursion_function(
%         validate_one,
%         [],
%         Return,
%         HeadName,
%         TailName,
%         [],
%         Condition,
%         [
%             {erl_syntax:atom(true), erl_syntax:atom(true)},
%             {
%                 erl_syntax:underscore(), 
%                 erl_syntax:application(
%                     erl_syntax:atom(FunctionName),
%                     [erl_syntax:variable(TailName)])
%             }
%         ]
%     ).

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
