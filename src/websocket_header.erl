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
%------------------------------------------------------------------------------
% FieldList -> {WebSocketDraft, request|response}|{error, Reason}
define(FieldList) when is_list(FieldList) ->
	case is_ws_header(FieldList)  of
		true ->
			define_1(FieldList);
		false ->
			{error, invalid_header}
	end;
define(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
define_1(FieldList) ->
	case find(?WS_REASON_PHRASE, FieldList) of
		{found, Reason} ->
			define_response(Reason, FieldList);
		notfound ->
			define_2(FieldList)
	end.
%------------------------------------------------------------------------------
is_ws_header(FieldList) ->
	case [find(?WS_CONNECTION, FieldList), find(?WS_UPGRADE, FieldList)] of
		[{found, ?WS_CON_VAL}, {found, UpgVal}] ->
			case string:to_lower(UpgVal) == string:to_lower(?WS_UPG_VAL) of
			       true -> true;
				_   -> false
			end;
		_ ->
			false
	end.
%------------------------------------------------------------------------------
define_2(FieldList) ->
	case find(?WS_METHOD, FieldList) of
		{found, "GET"} ->
			define_request(FieldList);
		{notfound, notfound} ->
			{error, invalid_header}
	end.
%------------------------------------------------------------------------------
define_response(Reason, _FieldList) ->
	case Reason of
		?HIXIE75_REASON_VAL ->
			% validate_hixie75_response();
			{?HIXIE75, response};
		?HIXIE76_REASON_VAL ->
			% validate_hixie76_response();
			{?HIXIE76, response};
		?HYBI_REASON_VAL ->
			% validate_hybi_response();
			{?HYBI, response};
		_ ->
			{error, invalid_response}
	end.
%------------------------------------------------------------------------------
define_request(FieldList) ->
	case find(?WS_ORIGIN, FieldList) of
		{found, _} ->
			define_request_hixie(FieldList);
		notfound ->
			define_request_hybi(FieldList)
	end.
%------------------------------------------------------------------------------
define_request_hixie(FieldList) ->
	case [find(?HIXIE76_KEY1, FieldList),find(?HIXIE76_KEY2, FieldList)] of
		[notfound, notfound] ->
			{?HIXIE75, request};
		[{found, _}, {found, _}] ->
			{?HIXIE76, request};
		_ ->
			{error, invalid_request_hixie}
	end.
%------------------------------------------------------------------------------
define_request_hybi(FieldList) ->
	case [ 
		find(?HYBI_KEY,     FieldList),
		find(?HYBI_ORIGIN,  FieldList),
		find(?HYBI_VERSION, FieldList) 
	] of
		[{found, _}, {found, _}, {found, _}] ->
			{?HYBI, request};
		_ ->
			{error, invalid_request_hybi}
	end.
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
