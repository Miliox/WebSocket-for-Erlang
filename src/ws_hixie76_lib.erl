%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 05/23/11 13:29:53 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(ws_hixie76_lib).
-author("elmiliox@gmail.com").
-vsn(1).

-include("ws_protocol_header.hrl").

-export([gen_response/1]).

%------------------------------------------------------------------------------
gen_response(Request) ->
	case catch(gen_response_0(Request)) of
		Response when is_list(Response) ->
			Response;
		_ ->
			{error, invalid_request}
	end.
%------------------------------------------------------------------------------
gen_response_0(Request) ->
	{found, Uri} = ws_header:find(?HIXIE76_URI, Request),
	{found, Host} = ws_header:find(?HIXIE76_HOST, Request),
	{found, Origin} = ws_header:find(?HIXIE76_ORIGIN_REQ, Request),
	
	Response = [
		{?HIXIE76_STATUS_CODE,   "101"},
		{?HIXIE76_REASON_PHRASE, ?HIXIE76_REASON_VAL},
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"},
		{?HIXIE76_ORIGIN_RES, Origin},
		{?HIXIE76_LOCATION, "ws://"++ Host ++ Uri}],

	gen_response_1(Response, Request).
%------------------------------------------------------------------------------
gen_response_1(Response, Request) ->
	case ws_header:find(?HIXIE76_PROTOCOL, Request) of
		{found, Protocol} ->
			gen_response_2( Response ++ 
				[{?HIXIE76_PROTOCOL, Protocol}], Request);
		notfound ->
			gen_response_2(Response, Request)
	end.
%------------------------------------------------------------------------------
gen_response_2(Response, Request) ->
	Key = resolve_challege(Request),
	Response ++ [{undefined, []},{undefined, Key}].
%------------------------------------------------------------------------------
resolve_challege(Request) ->
	{found, EncKey1} = ws_header:find(?HIXIE76_KEY1, Request),
	{found, EncKey2} = ws_header:find(?HIXIE76_KEY2, Request),
	{undefined, StrKey3} = lists:last(Request),

	Key1 = decode_key(EncKey1),
	Key2 = decode_key(EncKey2),
	Key3 = list_to_binary(StrKey3),

	binary_to_list(
		erlang:md5(
			<<Key1:32, Key2:32, Key3/binary>>)).
%------------------------------------------------------------------------------
decode_key(K) ->
	{Number, []} = string:to_integer([I|| I <- K, I >= $0, I =< $9]),
	TotalSpaces  = length([Sp|| Sp <- K, Sp == $  ]),

	trunc(Number / TotalSpaces).
%------------------------------------------------------------------------------

