-module(erltity_generator_utils).

%%% EXTERNAL EXPORTS
-export([
    atom_capitalize/1,
    capitalize/1,
    check_expression/4,
    check_expression/6,
    db_call_function/4,
    export/1,
    tail_recursion_function/8,
    tail_recursion_function/9,
    tail_recursion_function/10,
    parse_element/1,
    wrap_in_zero_arity_fun/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
atom_capitalize(Atom) ->
    binary_to_atom(capitalize(atom_to_binary(Atom))).

capitalize(<<H, T/binary>>) when H >= $a, H =< $z ->
    <<(H - 32), T/binary>>;
capitalize(Bin) ->
    Bin.

check_expression(Expression, ReasonFails, Expected, VariableName) ->
    check_expression(Expression, erl_syntax:atom(true), erl_syntax:atom(false), ReasonFails, Expected, VariableName).

check_expression(Expression, PassedCondition, FailCondition, ReasonFails, Expected, VariableName) ->
    erl_syntax:case_expr(
        Expression,
        [
            erl_syntax:clause(
                [PassedCondition],
                [],
                [erl_syntax:atom(true)]
            ),
            erl_syntax:clause(
                [FailCondition],
                [],
                [erl_syntax:tuple(
                    [
                        erl_syntax:atom(error),
                        erl_syntax:tuple(
                            [   
                                erl_syntax:atom(ReasonFails),
                                Expected,
                                erl_syntax:variable(VariableName)
                            ]
                        )   
                    ]
                )]   
            )
        ]
    ).

db_call_function(Module, FunctionName, EntityName, Args) ->
    ParsedVariables = lists:map(fun erl_syntax:variable/1, Args),
    erl_syntax:function(
        erl_syntax:atom(FunctionName), 
        [erl_syntax:clause(
            ParsedVariables,
            none,
            [
                erl_syntax:application(
                    erl_syntax:atom(Module), 
                    erl_syntax:atom(FunctionName), 
                    [erl_syntax:atom(EntityName)] ++ 
                    ParsedVariables
                )
            ]
        )]
    ).

export(List) ->
    erl_syntax:attribute(
        erl_syntax:atom(export),
        [
            erl_syntax:list(
                lists:map(
                    fun({FunName, Arity}) ->
                        erl_syntax:arity_qualifier(
                            erl_syntax:atom(FunName),
                            erl_syntax:integer(Arity)
                        )
                    end,
                    List
                )
            )
        ]
    ).

tail_recursion_function(FunctionName, Args, Return, HeadName, TailName, PrevFunctions, Condition, ClauseReturn) ->
ParseArgs = lists:map(fun erl_syntax:variable/1, Args),
 erl_syntax:function(
    erl_syntax:atom(FunctionName),
    [
        erl_syntax:clause(
            [
                erl_syntax:nil()
            ],
            [],
            [Return]
        ),
        erl_syntax:clause(
            ParseArgs ++
            [
                erl_syntax:cons(
                    erl_syntax:variable(HeadName),
                    erl_syntax:variable(TailName)
                )
            ],
            [],
            PrevFunctions ++ [
                erl_syntax:case_expr(
                    Condition,
                    lists:map(fun({Clause, ReturnValue}) -> 
                        erl_syntax:clause(
                            [Clause],
                            [],
                            [
                               ReturnValue
                            ]
                        ) end,
                        ClauseReturn
                    )
                )
            ]
        )
    ]
).

tail_recursion_function(FunctionName, Return, Head, TailName, PrevFunctions, Condition, Clause1, Clause2, FailReturn) ->
 erl_syntax:function(
    erl_syntax:atom(FunctionName),
    [
        erl_syntax:clause(
            [
                erl_syntax:nil()
            ],
            [],
            [Return]
        ),
        erl_syntax:clause(
            [
                erl_syntax:cons(
                    Head,
                    erl_syntax:variable(TailName)
                )
            ],
            [],
            PrevFunctions ++ [
                erl_syntax:case_expr(
                    Condition,
                    [
                        erl_syntax:clause(
                            [Clause1],
                            [],
                            [
                                erl_syntax:application(
                                    erl_syntax:atom(FunctionName),
                                    [erl_syntax:variable(TailName)]
                                )
                            ]
                        ),
                        erl_syntax:clause(
                            [Clause2],
                            [],
                            [
                                FailReturn
                            ]
                        )
                    ]
                )
            ]
        )
    ]
).

tail_recursion_function(FunctionName, Args, Return, HeadName, TailName, PrevFunctions, Condition, Clause1, Clause2, FailReturn) ->
ParseArgs = lists:map(fun erl_syntax:variable/1, Args),
 erl_syntax:function(
    erl_syntax:atom(FunctionName),
    [
        erl_syntax:clause(
            [
                erl_syntax:underscore(),
                erl_syntax:nil()
            ],
            [],
            [Return]
        ),
        erl_syntax:clause(
            ParseArgs ++
            [
                erl_syntax:cons(
                    erl_syntax:variable(HeadName),
                    erl_syntax:variable(TailName)
                )
            ],
            [],
            PrevFunctions ++ [
                erl_syntax:case_expr(
                    Condition,
                    [
                        erl_syntax:clause(
                            [Clause1],
                            [],
                            [
                                erl_syntax:application(
                                    erl_syntax:atom(FunctionName),
                                    ParseArgs ++ [erl_syntax:variable(TailName)]
                                )
                            ]
                        ),
                        erl_syntax:clause(
                            [Clause2],
                            [],
                            [
                                FailReturn
                            ]
                        )
                    ]
                )
            ]
        )
    ]
).

parse_element(Element) when is_binary(Element) ->
    erl_syntax:binary([
        erl_syntax:binary_field(erl_syntax:string(binary_to_list(Element)))
    ]);
parse_element(Element) ->
    erl_syntax:abstract(Element).

wrap_in_zero_arity_fun(Expr) ->
    erl_syntax:fun_expr([
    erl_syntax:clause([], [], [Expr])
]).

