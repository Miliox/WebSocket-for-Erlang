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

-define(RE_LINES, "[\r\n]+").
-define(RE_LINES_OPT, [{return, list}]).

-define(RE_REQ, "^([A-Z]+) (/[^ ]*) HTTP/1.1$").
-define(RE_FIELD, "^([^: ]+): ([^$]+)$").
-define(RE_FIELD_OPT, [{capture, all_but_first, list}]).
%------------------------------------------------------------------------------
parse_request(WebSocketHeader) when is_binary(WebSocketHeader) ->
	parse_request(binary_to_list(WebSocketHeader));
parse_request(WebSocketHeader) ->
	[WsRequest|LineFields] = 
		re:split(WebSocketHeader, ?RE_LINES, ?RE_LINES_OPT),

	RequestField = map_request(WsRequest),
	Fields = map_field(LineFields),
	RequestField ++ Fields.
%------------------------------------------------------------------------------
map_request(Line) ->
	case re:run(Line, ?RE_REQ, ?RE_FIELD_OPT) of 
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

		
