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
-vsn(2).

-export([parse_request/1]).
-include("websocket_header.hrl").

%------------------------------------------------------------------------------
% HTTP Header -> [{Key, Value}, ...]
parse_request(Header) when is_binary(Header) ->
	parse_request(binary_to_list(Header));
parse_request(Header) when is_list(Header) ->
	[RequestLine|FieldLines] = 
		re:split(Header, ?RE_LINES, ?RE_LINES_OPT),

	RequestField = request_field_list(RequestLine),
	HeaderFields = map_field(FieldLines),

	RequestField ++ HeaderFields;
parse_request(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
request_field_list(Line) ->
	case re:run(Line, ?RE_REQ, ?RE_REQ_OPT) of 
		{match, [Method, Path]} ->
			[field(method, Method), field(path, Path)];
		nomatch ->
			erlang:error("invalid-request-syntax")
	end.
%------------------------------------------------------------------------------
map_field(Lines) ->
	Line2Field = fun(Line) -> line_to_field(Line) end,
	lists:map(Line2Field, Lines).
%------------------------------------------------------------------------------
line_to_field(Line) ->
	case re:run(Line, ?RE_FIELD, ?RE_FIELD_OPT) of
		{match, [Name, Value]} ->
			field(Name, Value);
		nomatch ->
			field(undefined, Line)
	end.
%------------------------------------------------------------------------------
field(K, V) -> {K, V}.
