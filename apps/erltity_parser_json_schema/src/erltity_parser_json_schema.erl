-module(erltity_parser_json_schema).

%%% EXTERNAL EXPORTS
-export([
    parse/1,
    parse_from_file/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(Json) -> Schema when
    Json :: map(),
    Schema :: map().
parse(Json) ->
    {SchemaVersion, NewJson} = maps:take(<<"$schema">>, Json),
    Parser = parser(SchemaVersion),
    Parser:parse(NewJson).

parse_from_file(Path) ->
    {ok, Data} = file:read_file(Path),
    {ok, Json} = njson:decode(Data),
    parse(Json).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
parser(<<"http://json-schema.org/draft-07/schema#">>) ->
    erltity_parser_json_schema_draft_07.
