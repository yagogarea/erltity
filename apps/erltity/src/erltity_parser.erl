-module(erltity_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
parse(SchemaPath) ->
    BaseName = filename:basename(binary_to_list(SchemaPath)),
    case string:tokens(BaseName, ".") of
        [SchemaName, "json"] ->
            {list_to_atom(SchemaName), erltity_parser_json_schema:parse_from_file(SchemaPath)};
        [SchemaName | Rest ] ->
            case lists:last(Rest) of
                "json" ->
                    {list_to_atom(SchemaName), erltity_parser_json_schema:parse_from_file(SchemaPath)};
                _ ->
                    {error, unsupported_file_type}
            end;
        _ ->
            {error, unsupported_file_type}
    end.
