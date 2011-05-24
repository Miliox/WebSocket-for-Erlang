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

-export([gen_response/1, make_trial/0]).
-define(ASCII, 16#7F).
-define(BYTE,  16#FF).
-define(KEY3_SIZE, 8).
-define(MAX_SPACE, 16).
-define(INT4, 16#7FFFFFFF).
-define(NOSPACE, 0).

-define(VALID_CHAR, 
	"abcdefghijklmnopqrstuvwxyz" ++
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
	"'\"\\{}[]?.,<>|+-*%!@&^$#").
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
	Scheme = "ws://",
	{found, Uri} = ws_header:find(?HIXIE76_URI, Request),
	{found, Host} = ws_header:find(?HIXIE76_HOST, Request),
	{found, Origin} = ws_header:find(?HIXIE76_ORIGIN_REQ, Request),
	
	Location = Scheme ++ Host ++ Uri,

	Response = [
		{?HIXIE76_STATUS_CODE,   "101"},
		{?HIXIE76_REASON_PHRASE, ?HIXIE76_REASON_VAL},
		{?HIXIE76_UPGRADE,    ?HIXIE76_UPG_VAL},
		{?HIXIE76_CONNECTION, ?HIXIE76_CON_VAL},
		{?HIXIE76_ORIGIN_RES, Origin},
		{?HIXIE76_LOCATION,   Location}],

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
	Key = resolve_trial(Request),
	Response ++ [{undefined, []},{undefined, Key}].
%------------------------------------------------------------------------------
resolve_trial(Request) when is_list(Request)->
	{found, K1} = ws_header:find(?HIXIE76_KEY1, Request),
	{found, K2} = ws_header:find(?HIXIE76_KEY2, Request),
	{undefined, K3} = lists:last(Request),
	
	resolve_trial({K1, K2, K3});
resolve_trial({EncKey1, EncKey2, StrKey3}) ->
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
make_trial() ->
	random:seed(now()),

	K1 = random_encode_key(),
	K2 = random_encode_key(),
	K3 = random_key3(),
	Solved = resolve_trial({K1, K2, K3}),

	{K1, K2, K3, Solved}.
%------------------------------------------------------------------------------
random_encode_key() ->
	Number = random:uniform(?INT4),
	Spaces = random:uniform(?MAX_SPACE),
	Key = Number * Spaces,

	case (Key =< ?INT4) of
		true ->
			encode_key(Number, Spaces);
		false ->
			random_encode_key()
	end.
%------------------------------------------------------------------------------
encode_key(N, Spaces) 
when is_integer(N) andalso Spaces >= 0 ->
	[Int|Rest] = integer_to_list(N),
	Reverse = lists:reverse(Rest),

	[Int|encode_key_1(Reverse, Spaces, [])].
%------------------------------------------------------------------------------
encode_key_1([], ?NOSPACE, Buffer) ->
	lists:reverse(Buffer);
encode_key_1([Int|Rest]=Number, ?NOSPACE, Buffer) ->
	case random:uniform() of
		_Trash when _Trash < 0.5 ->
			encode_key_1(Number, ?NOSPACE, [random_char()|Buffer]);
		_Int ->
			encode_key_1(Rest, ?NOSPACE, [Int|Buffer])
	end;
encode_key_1([], Spaces, Buffer) ->
	case random:uniform() of
		_Trash when _Trash < 0.5 ->
			encode_key_1([], Spaces, [random_char()|Buffer]);
		_Space ->
			encode_key_1([], (Spaces-1), [$ |Buffer])
	end;
encode_key_1([Int|Rest]=Number, Spaces, Buffer) ->
	case random:uniform() of
		_Trash when _Trash < 0.33 ->
			encode_key_1(Number, Spaces, [random_char()|Buffer]);
		_Space when _Space < 0.66 ->
			encode_key_1(Number, (Spaces-1), [$ |Buffer]);
		_Int ->
			encode_key_1(Rest, Spaces, [Int|Buffer])
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
random_key3() ->
	random_key3(?KEY3_SIZE, []).
%------------------------------------------------------------------------------
random_key3(0, Buffer) ->
	Buffer;
random_key3(I, Buffer) ->
	Byte = random_byte(),
	random_key3(I-1, [Byte|Buffer]).
%------------------------------------------------------------------------------
random_byte() ->
	Byte = random:uniform(?BYTE),
	case Byte of
		BreakLine when 
			BreakLine == $\r orelse 
			BreakLine == $\n ->
				random_byte();
		_ ->
			Byte
	end.
