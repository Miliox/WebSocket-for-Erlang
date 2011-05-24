%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo que trata de aspectos especifico do Hixie76
%% Criado: 05/23/11 13:29:53 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(ws_hixie76_lib).
-author("elmiliox@gmail.com").
-vsn(1).

-include("ws_protocol_header.hrl").
-include("data_size.hrl").
%------------------------------------------------------------------------------
-import(erlang).
-import(lists).
-import(random).
-import(string).
-import(ws_header).
%------------------------------------------------------------------------------
-export([gen_response/1, make_trial/0, resolve_trial/1]).
-export([encode_key/2, decode_key/1]).
-export([random_encode_key/0, random_key3/0]).
%------------------------------------------------------------------------------
-define(SPACE, $ ).
-define(NO_SPACE,  0).
-define(MIN_SPACE, 1).
-define(MAX_SPACE, 16).
%------------------------------------------------------------------------------
-define(KEY3_SIZE, 8).
-define(VALID_PAD_CHAR, 
	"abcdefghijklmnopqrstuvwxyz" ++
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
	"'\"\\{}[]?.,<>|+-*%!@&^$#:;_").
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
	{undefined, Key3} = lists:last(Request),
	
	resolve_trial({K1, K2, Key3});
resolve_trial({EncKey1, EncKey2, Key3}) 
when 
	is_list(EncKey1) andalso
	is_list(EncKey2) andalso
	is_list(Key3) ->

	Key1 = decode_key(EncKey1),
	Key2 = decode_key(EncKey2),

	resolve_trial({Key1, Key2, Key3});
resolve_trial({Key1, Key2, Key3})
when 
	is_integer(Key1) andalso 
	is_integer(Key2) andalso
	is_list(Key3)  andalso
	length(Key3) == ?KEY3_SIZE ->
	BinKey3 = list_to_binary(Key3),

	binary_to_list(
		erlang:md5(
			<<Key1:32, Key2:32, BinKey3/binary>>));
resolve_trial(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
decode_key(K) ->
	{Number, []} = string:to_integer([I|| I <- K, I >= $0 andalso I =< $9]),
	TotalSpaces  = length([Sp|| Sp <- K, Sp == ?SPACE ]),

	trunc(Number / TotalSpaces).
%------------------------------------------------------------------------------
make_trial() ->
	random:seed(now()),

	K1 = random_encode_key(),
	K2 = random_encode_key(),
	K3 = random_key3(),
	Solution = resolve_trial({K1, K2, K3}),

	{K1, K2, K3, Solution}.
%------------------------------------------------------------------------------
random_encode_key() ->
	Key    = random:uniform(?INT4),
	Spaces = random:uniform(?MAX_SPACE) + ?MIN_SPACE,

	case ((Key * Spaces) < ?INT4) of
		true ->
			encode_key(Key, Spaces);
		false ->
			random_encode_key()
	end.
%------------------------------------------------------------------------------
encode_key(Key, Spaces) 
when 
	is_integer(Key) andalso 
	Spaces >= ?MIN_SPACE ->

	EncKey     = Key * Spaces,
	[Int|Rest] = integer_to_list(EncKey),

	[Int|encode_key_1(Rest, Spaces, [])];
encode_key(_, _) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
encode_key_1([], ?NO_SPACE, Buffer) ->
	lists:reverse(Buffer);
encode_key_1([Int|Rest]=Number, ?NO_SPACE, Buffer) ->
	case random:uniform() of
		_Pad when _Pad < 0.5 ->
			encode_key_1(Number, ?NO_SPACE, [pad_char()|Buffer]);
		_Int ->
			encode_key_1(Rest,   ?NO_SPACE, [Int|Buffer])
	end;
encode_key_1([], Spaces, Buffer) ->
	case random:uniform() of
		_Pad when _Pad < 0.5 ->
			encode_key_1([], Spaces,     [pad_char()|Buffer]);
		_Space ->
			encode_key_1([], (Spaces-1), [?SPACE|Buffer])
	end;
encode_key_1([Int|Rest]=Number, Spaces, Buffer) ->
	case random:uniform() of
		_Pad when _Pad < 0.33 ->
			encode_key_1(Number, Spaces,     [pad_char()|Buffer]);
		_Space when _Space < 0.66 ->
			encode_key_1(Number, (Spaces-1), [?SPACE|Buffer]);
		_Int ->
			encode_key_1(Rest,   Spaces,     [Int|Buffer])
	end.
%------------------------------------------------------------------------------
pad_char() ->
	Char = random:uniform(?ASCII),
	case lists:member(Char, ?VALID_PAD_CHAR) of
		true ->
			Char;
		false ->
			pad_char()
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
