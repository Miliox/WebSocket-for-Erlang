%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo que trata de aspectos especifico do Hixie76
%% Criado: 05/23/11 13:29:53 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

%------------------------------------------------------------------------------
-module(wslib.hixie76).
-author("elmiliox@gmail.com").
-vsn(2).
%------------------------------------------------------------------------------
-include("data_size.hrl").
-include("wslib/hixie76.hrl").
-include("re/subprotocol.hrl").
-include("protocol_header.hrl").
%------------------------------------------------------------------------------
-import(lists).
-import(erlang).
-import(random).
-import(string).
-import(wslib.url).
-import(wslib.header).
-import(wslib.hixie_frame).
%------------------------------------------------------------------------------
-export([gen_request/2, gen_request/3, gen_response/1, gen_response/2]).
-export([make_trial/0, resolve_trial/1]).
-export([encode_key/1, encode_key/2, decode_key/1]).
-export([random_encode_key/0, random_key3/0]).
-export([frame/1, unframe/1, unframe/2]).
%------------------------------------------------------------------------------
gen_request(Url, FromUrl) ->
	gen_request(Url, FromUrl, []).
%------------------------------------------------------------------------------
gen_request(Url, FromUrl, SubProtocol) ->
	{_, _, Host, _, Uri} = url:parse(Url),
	{K1, K2, K3, S} = make_trial(),
	Origin = FromUrl,

	Request = [
		{?HIXIE76_METHOD, ?HIXIE76_MET_VAL}, 
		{?HIXIE76_URI, Uri}, 
		{?HIXIE76_UPGRADE, ?HIXIE76_UPG_VAL},
		{?HIXIE76_CONNECTION, ?HIXIE76_CON_VAL},
		{?HIXIE76_KEY2, K2},
		{?HIXIE76_HOST, Host},
		{?HIXIE76_KEY1, K1},
		{?HIXIE76_ORIGIN_REQ, Origin}],

	gen_request_1(Request, SubProtocol, K3, S).
%------------------------------------------------------------------------------
gen_request_1(Request, SubProtocol, K3, S) ->
	case  string:join(SubProtocol, ", ") of
		[] ->
			gen_request_2(Request, K3, S);
		ProtField ->
			gen_request_2(Request ++ 
				[{?HIXIE76_PROTOCOL, ProtField}], K3, S)
	end.
%------------------------------------------------------------------------------
gen_request_2(Request, K3, Solution) ->
	{Request ++ [ {undefined, []}, {undefined, K3} ], Solution}.
%------------------------------------------------------------------------------
gen_response(Request) ->
	gen_response(Request, normal).
gen_response(Request, Mode) ->
	case catch(gen_response_1(Request, Mode)) of
		Response when is_list(Response) ->
			Response;
		_ ->
			{error, invalid_request}
	end.
%------------------------------------------------------------------------------
gen_response_1(Request, Mode) ->
	{found, Origin} = header:find(?HIXIE76_ORIGIN_REQ, Request),
	
	Location = location_from_request(Mode, Request),
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
	case header:find(?HIXIE76_PROTOCOL, Request) of
		{found, ProtocolList} ->
			Protocol = 
				header:resolve_subprotocol(ProtocolList),
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
	{found, K1} = header:find(?HIXIE76_KEY1, Request),
	{found, K2} = header:find(?HIXIE76_KEY2, Request),
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
	is_list(Key3)    andalso
	length(Key3) == ?KEY3_SIZE ->

	BinKey3 = list_to_binary(Key3),
	binary_to_list(
		erlang:md5(
			<<Key1:32, Key2:32, BinKey3/binary>>));
resolve_trial(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
decode_key(K) when is_list(K) ->
	{Number, []} = string:to_integer([I|| I <- K, I >= $0 andalso I =< $9]),
	TotalSpaces  = length([Sp|| Sp <- K, Sp == ?SPACE ]),

	Number div TotalSpaces;
decode_key(_) ->
	erlang:error(badarg).
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
	Key = random:uniform(?INT4),
	encode_key(Key).
%------------------------------------------------------------------------------
encode_key(Key) 
when 
	is_integer(Key) andalso Key >= 0 ->

	MaxSpaces = ?INT4 div Key,
	case random:uniform(MaxSpaces) of
		Spaces when (Spaces > ?MIN_SPACE) ->
			encode_key(Key, Spaces);
		_ ->
			encode_key(Key, ?MIN_SPACE)
	end;
encode_key(_) ->
	erlang:error(badarg).
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
random_key3(0, Key3) ->
	Key3;
random_key3(I, Buffer) ->
	random_key3(I-1, [random_byte()|Buffer]).
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
%------------------------------------------------------------------------------
location_from_request(Mode, Request) ->
	Scheme = case Mode of
		normal -> "ws://";
		secure -> "wss://"
	end,
	{found, Uri} = header:find(?HIXIE76_URI, Request),
	{found, Host} = header:find(?HIXIE76_HOST, Request),

	Scheme ++ Host ++ Uri.
%------------------------------------------------------------------------------
get_subprotocol(Header) ->
	case header:find(?HIXIE76_PROTOCOL, Header) of
		{found, SubProtocol} ->
			SubProtocol;
		notfound ->
			nil
	end.
%------------------------------------------------------------------------------
frame(Frame) ->
	hixie_frame:frame(Frame).
%------------------------------------------------------------------------------
unframe(Stream) ->
	hixie_frame:unframe(Stream).
%------------------------------------------------------------------------------
unframe(Stream, Context) ->
	hixie_frame:unframe(Stream, Context).
%------------------------------------------------------------------------------

