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

-export([parse_request/1, parse_response/1]).
-include("websocket_header.hrl").

%------------------------------------------------------------------------------
% HTTP Header -> [{Key, Value}, ...]
parse_request(Header) when is_binary(Header) ->
	parse_request(binary_to_list(Header));
parse_request(Header) when is_list(Header) ->
	[RequestLine|FieldLines] = break_lines(Header),

	RequestFields = request_field_list(RequestLine),
	HeaderFields  = map_field(FieldLines),

	RequestFields ++ HeaderFields;
parse_request(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
parse_response(Header) when is_binary(Header) ->
	parse_response(binary_to_list(Header));
parse_response(Header) when is_list(Header) ->
	[StatusLine|FieldLines] = break_lines(Header),
	
	StatusFields = response_field_list(StatusLine),
	HeaderFields = map_field(FieldLines),

	StatusFields ++ HeaderFields;
parse_response(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
break_lines(Header) ->
		re:split(Header, ?RE_LINES, ?RE_LINES_OPT).
%------------------------------------------------------------------------------
request_field_list(Line) ->
	case re:run(Line, ?RE_REQ, ?RE_REQ_OPT) of 
		{match, [Method, Path]} ->
			[field(method, Method), field(path, Path)];
		nomatch ->
			erlang:error("invalid-request-syntax")
	end.
%------------------------------------------------------------------------------
response_field_list(Line) ->
	case re:run(Line, ?RE_RES, ?RE_RES_OPT) of 
		{match, [Status, Reason]} ->
			[field(status, Status), field(reason, Reason)];
		nomatch ->
			erlang:error("invalid-request-syntax")
	end.
%------------------------------------------------------------------------------
map_field(Lines) ->
	[Field||
		L <- Lines,
		Field <- [line_to_field(L)]
	].
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
