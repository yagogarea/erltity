-module(erltity_generator).

%%% EXTERNAL EXPORTS
-export([
    generate/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
generate(DbDriverModule, SchemaName) ->
    ModAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(SchemaName)]),
    ExportAttr = erl_syntax:attribute(
        erl_syntax:atom(export), [
            erl_syntax:list(
                [
                    erl_syntax:arity_qualifier(erl_syntax:atom(create), erl_syntax:integer(1)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(update), erl_syntax:integer(2)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(delete), erl_syntax:integer(1)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(find), erl_syntax:integer(1)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(find), erl_syntax:integer(2))
                ]
            )
        ]
    ),
    CallbackAttr = erl_syntax:attribute(
        erl_syntax:atom(behaviour), [
        erl_syntax:atom(erltity)
        ]
    ),
    CreateAttr = attribute(DbDriverModule, SchemaName, create, 'CreateRequest'),
    UpdateAttr = attribute(DbDriverModule, SchemaName, update, 'ID', 'UpdateRequest'),
    DeleteAttr = attribute(DbDriverModule, SchemaName, delete, 'ID'),
    FindAttr = attribute(DbDriverModule, SchemaName, find, 'ID'),
    MultiFindAttr =  attribute(DbDriverModule, SchemaName, find, 'Filters', 'Opts'),
    Attrs = [ModAttr, CallbackAttr, ExportAttr, CreateAttr, UpdateAttr, DeleteAttr, FindAttr, MultiFindAttr],
    [erl_syntax:revert(AST) || AST <- Attrs].


%%%-----------------------------------------------------------------------------
%%% INTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
attribute(DbDriverModule, SchemaName, FunctionName, Variable) ->
    erl_syntax:function(
        erl_syntax:atom(FunctionName), 
        [erl_syntax:clause(
            [erl_syntax:variable(Variable)],
            none,
            [
                erl_syntax:application(
                    erl_syntax:atom(DbDriverModule), 
                    erl_syntax:atom(FunctionName), 
                    [
                        erl_syntax:atom(SchemaName),
                        erl_syntax:variable(Variable)
                    ]
                )
            ]
        )]
    ).

attribute(DbDriverModule, SchemaName, FunctionName, Variable1, Variable2) ->
    erl_syntax:function(
        erl_syntax:atom(FunctionName), 
        [erl_syntax:clause(
            [erl_syntax:variable(Variable1), erl_syntax:variable(Variable2)],
            none,
            [
                erl_syntax:application(
                    erl_syntax:atom(DbDriverModule), 
                    erl_syntax:atom(FunctionName), 
                    [
                        erl_syntax:atom(SchemaName),
                        erl_syntax:variable(Variable1),
                        erl_syntax:variable(Variable2)
                    ]
                )
            ]
        )]
    ).

