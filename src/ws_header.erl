%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo que abstrai as operacoes sobre o cabecalho WebSocket
%% Criado: 05/18/11 22:28:39 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(ws_header).
-author("elmiliox@gmail.com").
-vsn(3).
%------------------------------------------------------------------------------
-export([parse/1, type/1, find/2, to_string/1, resolve_subprotocol/1]).
%------------------------------------------------------------------------------
-include("ws_protocol_header.hrl").
-include("ws_re_header.hrl").
-include("ws_re_subprotocol.hrl").
%------------------------------------------------------------------------------
-define(DEFINED_HEADER(Draft, Type), {ok, {Draft, Type}}).
-define(ERROR_HEADER, {error, invalid_header}).
%------------------------------------------------------------------------------
-define(FIRST, 1).
-define(SPACE, $ ).
-define(COLON, $:).
-define(SEPARATOR(Field), [?COLON|[?SPACE|Field]]).
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
			[field(?WS_METHOD, Method),
			 field(?WS_URI, Path)];
		nomatch ->
			parse_start_line_1(L)
	end.
%------------------------------------------------------------------------------
parse_start_line_1(L) ->
	case re:run(L, ?RE_RES, ?RE_RES_OPT) of 
		{match, [Status, Reason]} ->
			[field(?WS_STATUS_CODE, Status),
			 field(?WS_REASON_PHRASE, Reason)];
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
	line_to_field(Line, []).
%------------------------------------------------------------------------------
line_to_field([], ReverseLine) ->
	Line = lists:reverse(ReverseLine),
	field(undefined, Line);
line_to_field(?SEPARATOR(Value), ReverseName) ->
	Name = lists:reverse(ReverseName),
	field(Name, Value);
line_to_field([Char|TailLine], ReverseName) ->
	line_to_field(TailLine, [Char|ReverseName]).
%------------------------------------------------------------------------------
field(K, V) -> {K, V}.
%------------------------------------------------------------------------------
% FList -> {ok, {WebSocketDraft, request|response}}|{error, Reason}
type(FList) when is_list(FList) ->
	case is_ws_header(FList)  of
		true ->
			type_1(FList);
		false ->
			?ERROR_HEADER
	end;
type(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
type_1(FList) ->
	case find(?WS_REASON_PHRASE, FList) of
		{found, Reason} ->
			type_response(Reason, FList);
		notfound ->
			type_2(FList)
	end.
%------------------------------------------------------------------------------
type_2(FList) ->
	case find(?WS_METHOD, FList) of
		{found, ?WS_MET_VAL} ->
			type_request(FList);
		{notfound, notfound} ->
			?ERROR_HEADER
	end.
%------------------------------------------------------------------------------
type_request(FList) ->
	case find(?WS_ORIGIN, FList) of
		{found, _} ->
			type_request_hixie(FList);
		notfound ->
			type_request_hybi(FList)
	end.
%------------------------------------------------------------------------------
type_request_hixie(FList) ->
	case [find(?HIXIE76_KEY1, FList),find(?HIXIE76_KEY2, FList)] of
		[notfound, notfound] ->
			?DEFINED_HEADER(?HIXIE75, request);
		[{found, _}, {found, _}] ->
			?DEFINED_HEADER(?HIXIE76, request);
		_ ->
			?ERROR_HEADER
	end.
%------------------------------------------------------------------------------
type_request_hybi(FList) ->
	case [ 
		find(?HYBI_KEY,     FList),
		find(?HYBI_ORIGIN,  FList),
		find(?HYBI_VERSION, FList) 
	] of
		[{found, _}, {found, _}, {found, _}] ->
			?DEFINED_HEADER(?HYBI, request);
		_ ->
			?ERROR_HEADER
	end.
%------------------------------------------------------------------------------
type_response(Reason, FList) ->
	case Reason of
		?HIXIE75_REASON_VAL ->
			type_response_hixie75(FList);
		?HIXIE76_REASON_VAL ->
			type_response_hixie76(FList);
		?HYBI_REASON_VAL ->
			type_response_hybi(FList);
		_ ->
			?ERROR_HEADER
	end.
%------------------------------------------------------------------------------
type_response_hixie75(FList) ->
	case [
		find(?HIXIE75_ORIGIN_RES, FList),
		find(?HIXIE75_LOCATION, FList)
	] of
		[{found, _}, {found, _}] ->
			?DEFINED_HEADER(?HIXIE75, response);
		_ ->
			?ERROR_HEADER
	end.
%------------------------------------------------------------------------------
type_response_hixie76(FList) ->
	case [
		find(?HIXIE76_ORIGIN_RES, FList),
		find(?HIXIE76_LOCATION, FList)
	] of
		[{found, _}, {found, _}] ->
			?DEFINED_HEADER(?HIXIE76, response);
		_ ->
			?ERROR_HEADER
	end.
%------------------------------------------------------------------------------
type_response_hybi(FList) ->
	case find(?HYBI_ACCEPT, FList) of
		{found, _} ->
			?DEFINED_HEADER(?HYBI, response);
		_ ->
			?ERROR_HEADER
	end.
%------------------------------------------------------------------------------
is_ws_header(FList) ->
	case [find(?WS_CONNECTION, FList), find(?WS_UPGRADE, FList)] of
		[{found, ?WS_CON_VAL}, {found, U}] ->
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
% FList -> string()
to_string(FList) ->
	case type(FList) of
		{ok, {Draft, Type}} ->
			to_string_1(Draft, Type, FList);
		_ ->
			erlang:error(badarg)
	end.
%------------------------------------------------------------------------------
to_string_1(Draft, Type, FList) ->
	StartLine  = to_string_start_line(Draft, Type, FList),
	FieldLines = map_to_string_field(FList),

	string:join([StartLine|FieldLines], "\r\n").
%------------------------------------------------------------------------------
to_string_start_line(_Draft, request, FList) ->
	{found, Method} = find(?WS_METHOD, FList),
	{found, Uri}    = find(?WS_URI, FList),
	
	Method ++ " " ++ Uri ++ " HTTP/1.1";
to_string_start_line(?HIXIE75, response, _) ->
	"HTTP/1.1 101 " ++ ?HIXIE75_REASON_VAL;
to_string_start_line(?HIXIE76, response, _) ->
	"HTTP/1.1 101 " ++ ?HIXIE76_REASON_VAL;
to_string_start_line(?HYBI, response, _) ->
	"HTTP/1.1 101 " ++ ?HYBI_REASON_VAL;
to_string_start_line(_, _, _) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
map_to_string_field(FList) ->
	[String||
		Field  <- FList, 
		String <- [to_string_field(Field)], 
		is_list(String)].
%------------------------------------------------------------------------------
to_string_field(Field) ->
	case Field of
		{Name, Value} when is_list(Name) andalso is_list(Value) ->
			Name ++ ": " ++ Value;
		{undefined, Value} when is_list(Value) ->
			Value;
		_ ->
			ignore
	end.
%------------------------------------------------------------------------------
resolve_subprotocol(ProtocolList) ->
	lists:nth(?FIRST,
		re:split(ProtocolList, ?RE_PROT_SEP, ?RE_PROT_OPT)).
%------------------------------------------------------------------------------
