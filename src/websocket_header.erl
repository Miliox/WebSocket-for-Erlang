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

-export([parse/1, define/1, find/2]).
-include("websocket_header.hrl").
-include("websocket_protocol_header.hrl").

%------------------------------------------------------------------------------
% HTTP Header -> [{Key, Value}, ...]
parse(Header) when is_binary(Header) ->
	parse(binary_to_list(Header));
parse(Header) when is_list(Header) ->
	[StartLine|FieldLines] = break_lines(Header),

	StartFields  = parse_start_line(StartLine),
	HeaderFields = map_field(FieldLines),

	StartFields ++ HeaderFields;
parse(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
define(FieldList) when is_list(FieldList) ->
	erlang:error();
define(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
break_lines(Header) ->
	re:split(Header, ?RE_LINES, ?RE_LINES_OPT).
%------------------------------------------------------------------------------
parse_start_line(L) ->
	case re:run(L, ?RE_REQ, ?RE_REQ_OPT) of 
		{match, [Method, Path]} ->
			[field(?WS_METHOD, Method), field(?WS_URI, Path)];
		nomatch ->
			parse_start_line_1(L)
	end.
parse_start_line_1(L) ->
	case re:run(L, ?RE_RES, ?RE_RES_OPT) of 
		{match, [Status, Reason]} ->
			[field(?WS_STATUS_CODE, Status), field(?WS_REASON_PHRASE, Reason)];
		nomatch ->
			[field(undefined, L)]
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
%------------------------------------------------------------------------------
find(FName, FList) ->
	FNameIndex = 1,
	case lists:keyfind(FName, FNameIndex, FList) of
		{FName, Value} ->
			{found, Value};
		false -> 
			notfound
	end.
%------------------------------------------------------------------------------
