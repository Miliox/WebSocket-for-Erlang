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

-export([gen_response/1, make_challenge/0]).
-define(MAX_INT4, 16#7FFFFFFF).
-define(ASCII, 16#7F).
-define(BYTE,  16#FF).
-define(KEY3_SIZE, 8).

-define(VALID_CHAR, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*%!@&^$#").
%------------------------------------------------------------------------------
gen_response(Request) ->
	case catch(gen_response_1(Request)) of
		Response when is_list(Response) ->
			Response;
		_ ->
			{error, invalid_request}
	end.
%------------------------------------------------------------------------------
gen_response_1(Request) ->
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

	gen_response_2(Response, Request).
%------------------------------------------------------------------------------
gen_response_2(Response, Request) ->
	case ws_header:find(?HIXIE76_PROTOCOL, Request) of
		{found, Protocol} ->
			gen_response_3( Response ++ 
				[{?HIXIE76_PROTOCOL, Protocol}], Request);
		notfound ->
			gen_response_3(Response, Request)
	end.
%------------------------------------------------------------------------------
gen_response_3(Response, Request) ->
	Key = resolve_challege(Request),
	Response ++ [{undefined, []},{undefined, Key}].
%------------------------------------------------------------------------------
resolve_challege(Request) when is_list(Request)->
	{found, EncKey1} = ws_header:find(?HIXIE76_KEY1, Request),
	{found, EncKey2} = ws_header:find(?HIXIE76_KEY2, Request),
	{undefined, StrKey3} = lists:last(Request),
	
	resolve_challege({EncKey1, EncKey2, StrKey3});
resolve_challege({EncKey1, EncKey2, StrKey3}) ->
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
make_challenge() ->
	EncKey1 = gen_encoded_key(),
	EncKey2 = gen_encoded_key(),
	StrKey3 = gen_key3(),

	Solved = resolve_challege({EncKey1, EncKey2, StrKey3}),
	{EncKey1, EncKey2, StrKey3, Solved}.
%------------------------------------------------------------------------------
gen_encoded_key() ->
	Number = random:uniform(?MAX_INT4),
	Spaces = random:uniform(16),

	Key = Number * Spaces,
	case Key >= ?MAX_INT4 of
		true ->
			gen_encoded_key();
		false ->
			to_encoded_key(Number, Spaces)
	end.
%------------------------------------------------------------------------------
to_encoded_key(Number, Spaces) 
when is_integer(Number) andalso Spaces >= 0 ->
	RevNumber = lists:reverse(
		integer_to_list(Number)),
	to_encoded_key_1(RevNumber, Spaces, []).
%------------------------------------------------------------------------------
to_encoded_key_1([], 0, Str) ->
	lists:reverse(Str);
to_encoded_key_1(Number, 0, Str) ->
	case random:uniform() of
		_Low when _Low < 0.5 ->
			Char = random_char(),
			to_encoded_key_1(Number, 0, [Char|Str]);
		_Hig ->
			[Char|Rest] = Number,
			to_encoded_key_1(Rest, 0, [Char|Str])
	end;
to_encoded_key_1([], Spaces, Str) ->
	case random:uniform() of
		_Low when _Low < 0.5 ->
			Char = random_char(),
			to_encoded_key_1([], Spaces, [Char|Str]);
		_Hig ->
			Char = $ , 
			to_encoded_key_1([], Spaces - 1, [Char|Str])
	end;
to_encoded_key_1(Number, Spaces, Str) ->
	case random:uniform() of
		_Low when _Low < 0.33 ->
			Char = random_char(),
			to_encoded_key_1(Number, Spaces, [Char|Str]);
		_Mid when _Mid < 0.66 ->
			Char = $ ,
			to_encoded_key_1(Number, Spaces - 1, [Char|Str]);
		_Hig ->
			[Char|Rest] = Number,
			to_encoded_key_1(Rest, Spaces, [Char|Str])
	end.
%------------------------------------------------------------------------------
random_char() ->
	Char = random:uniform(?ASCII),
	case lists:member(Char, ?VALID_CHAR) of
		true ->
			Char;
		false ->
			random_char()
	end.
%------------------------------------------------------------------------------
gen_key3() ->
	gen_key3_1(?KEY3_SIZE, []).
%------------------------------------------------------------------------------
gen_key3_1(0, Buffer) ->
	Buffer;
gen_key3_1(N, Buffer) ->
	Byte = random_byte(),
	gen_key3_1(N-1, [Byte|Buffer]).
%------------------------------------------------------------------------------
random_byte() ->
	random:uniform(?BYTE).
