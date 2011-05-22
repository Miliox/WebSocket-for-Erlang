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
-include("websocket_regex_header.hrl").
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
%------------------------------------------------------------------------------
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
% FList -> {WebSocketDraft, request|response}|{error, Reason}
define(FList) when is_list(FList) ->
	case is_ws_header(FList)  of
		true ->
			define_1(FList);
		false ->
			{error, invalid_header}
	end;
define(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
define_1(FList) ->
	case find(?WS_REASON_PHRASE, FList) of
		{found, Reason} ->
			define_response(Reason, FList);
		notfound ->
			define_2(FList)
	end.
%------------------------------------------------------------------------------
define_2(FList) ->
	case find(?WS_METHOD, FList) of
		{found, ?WS_MET_VAL} ->
			define_request(FList);
		{notfound, notfound} ->
			{error, invalid_header}
	end.
%------------------------------------------------------------------------------
define_request(FList) ->
	case find(?WS_ORIGIN, FList) of
		{found, _} ->
			define_request_hixie(FList);
		notfound ->
			define_request_hybi(FList)
	end.
%------------------------------------------------------------------------------
define_request_hixie(FList) ->
	case [find(?HIXIE76_KEY1, FList),find(?HIXIE76_KEY2, FList)] of
		[notfound, notfound] ->
			{?HIXIE75, request};
		[{found, _}, {found, _}] ->
			{?HIXIE76, request};
		_ ->
			{error, invalid_request_hixie}
	end.
%------------------------------------------------------------------------------
define_request_hybi(FList) ->
	case [ 
		find(?HYBI_KEY,     FList),
		find(?HYBI_ORIGIN,  FList),
		find(?HYBI_VERSION, FList) 
	] of
		[{found, _}, {found, _}, {found, _}] ->
			{?HYBI, request};
		_ ->
			{error, invalid_request_hybi}
	end.
%------------------------------------------------------------------------------
define_response(Reason, _FList) ->
	case Reason of
		?HIXIE75_REASON_VAL ->
			{?HIXIE75, response};
		?HIXIE76_REASON_VAL ->
			{?HIXIE76, response};
		?HYBI_REASON_VAL ->
			{?HYBI, response};
		_ ->
			{error, invalid_response}
	end.
%------------------------------------------------------------------------------
is_ws_header(FList) ->
	case [find(?WS_CONNECTION, FList), find(?WS_UPGRADE, FList)] of
		[{found, ?WS_CON_VAL}, {found, UpgradeValue}] ->
			string:to_lower(U) == string:to_lower(?WS_UPG_VAL);
		_ ->
			false
	end.
%------------------------------------------------------------------------------
% find(FName, Flist) -> {found, FVAlue}|notfound
find(FName, FList) ->
	FNameIndex = 1,
	case lists:keyfind(FName, FNameIndex, FList) of
		{FName, Value} ->
			{found, Value};
		false -> 
			notfound
	end.
%------------------------------------------------------------------------------
