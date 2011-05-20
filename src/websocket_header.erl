%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo que abstrai as operacoes sobre o cabecalho WebSocket
%% Criado: 05/18/11 22:28:39 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(websocket_header).
-author("elmiliox@gmail.com").
-vsn(1).

-export([parse_request/1]).

-define(RE_HTTP_METHOD, "[A-Z]+").
-define(RE_HTTP_URI,   "/[^ ]*").
-define(RE_REQ, "^(" ++ ?RE_HTTP_METHOD ++ ") ("++ ?RE_HTTP_URI ++") HTTP/1.1$").

-define(RE_FIELD_NAME, "[^:]+").
-define(RE_FIELD_VALUE, "[^$]+").
-define(RE_FIELD, "^("++ ?RE_FIELD_NAME  ++"): ("++ ?RE_FIELD_VALUE ++")$").

-define(RE_LINES, "[\r\n]+").
-define(RE_LINES_OPT, [{return, list}]).
-define(RE_FIELD_OPT, [{capture, all_but_first, list}]).
-define(RE_REQ_OPT, ?RE_FIELD_OPT).
%------------------------------------------------------------------------------
parse_request(Header) when is_binary(Header) ->
	parse_request(binary_to_list(Header));
parse_request(Header) ->
	[RequestLine|FieldLines] = 
		re:split(Header, ?RE_LINES, ?RE_LINES_OPT),

	RequestField = map_request(RequestLine),
	HeaderFields = map_field(FieldLines),

	RequestField ++ HeaderFields.
%------------------------------------------------------------------------------
map_request(Line) ->
	case re:run(Line, ?RE_REQ, ?RE_REQ_OPT) of 
		{match, [Method, Path]} ->
			[{method, Method}, {path, Path}];
		nomatch ->
			erlang:error("invalid-request-syntax")
	end.
map_field(Lines) ->
	Line2Field = fun(Line) -> line_to_field(Line) end,

	lists:map(Line2Field, Lines).

line_to_field(Line) ->
	case re:run(Line, ?RE_FIELD, ?RE_FIELD_OPT) of
		{match, [Key, Value]} ->
			{Key, Value};
		nomatch ->
			{undefined, Line}
	end.

		
