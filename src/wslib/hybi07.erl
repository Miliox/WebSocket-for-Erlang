%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo especifico das operacoes do draft-hybi-07
%% Criado: 06/05/11 18:47:52 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(wslib.hybi07).
-author("elmiliox@gmail.com").
-vsn(1).
%------------------------------------------------------------------------------
-define(GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").
-define(DECODE_KEY_LEN, 16).
%------------------------------------------------------------------------------
-include("data_size.hrl").
-include("protocol_header.hrl").
%------------------------------------------------------------------------------
-import(crypto).
-import(base64).
-import(random).
-import(string).
-import(wslib.url).
-import(wslib.header).
%------------------------------------------------------------------------------
-export([make_trial/0, resolve_trial/1, random_key/0]).
-export([gen_request/2, gen_request/3]).
%------------------------------------------------------------------------------
resolve_trial(Request) when is_list(Request) ->
	{found, Key} = header:find(?HYBI_KEY, Request),
	resolve_trial_from_key(Key).
%------------------------------------------------------------------------------
resolve_trial_from_key(Key) ->
	binary_to_list( base64:encode( crypto:sha(Key ++ ?GUID) ) ).
%------------------------------------------------------------------------------
make_trial() ->
	Key = random_key(),
	Answer = resolve_trial_from_key(Key),
	{Key, Answer}.
%------------------------------------------------------------------------------
random_key() ->
	random_key_1([], ?DECODE_KEY_LEN).
%------------------------------------------------------------------------------
random_key_1(DecodeKey, 0) ->
	binary_to_list(base64:encode(DecodeKey));
random_key_1(DecodeKey, Len) ->
	Char = random:uniform(?BYTE),
	random_key_1([Char|DecodeKey], Len-1).
%------------------------------------------------------------------------------
gen_request(Url, FromUrl) ->
	gen_request(Url, FromUrl, []).
%------------------------------------------------------------------------------
gen_request(Url, FromUrl, SubProtocol) ->
	{_, _, Host, _, Uri} = url:parse(Url),
	{Key, Solution} = make_trial(),
	Origin = FromUrl,

	Request = [
		{?HYBI_METHOD, ?HYBI_MET_VAL},
		{?HYBI_URI,  Uri},
		{?HYBI_HOST, Host},
		{?HYBI_UPGRADE,    ?HYBI_UPG_VAL},
		{?HYBI_CONNECTION, ?HYBI_CON_VAL},
		{?HYBI_KEY,    Key},
		{?HYBI_ORIGIN, Origin}],

	gen_request_1(Request, SubProtocol, Solution).
%------------------------------------------------------------------------------
gen_request_1(Request, SubProtocol, Solution) ->
	case  string:join(SubProtocol, ", ") of
		[] ->
			gen_request_2(Request, Solution);
		ProtField ->
			gen_request_2(Request ++ 
				[{?HYBI_PROTOCOL, ProtField}], Solution)
	end.
%------------------------------------------------------------------------------
gen_request_2(Request, Solution) ->
	FullRequest = 
		Request ++ [ 
			{?HYBI_VERSION, "7"}, 
			{undefined, []}, 
			{undefined, []} ],

	{FullRequest, Solution}.
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
	{found, _} = header:find(?HYBI_URI, Request),
	{found, _} = header:find(?HYBI_HOST, Request),
	{found, _} = header:find(?HYBI_ORIGIN, Request),
	
	Response = [
		{?HYBI_STATUS_CODE, "101"},
		{?HYBI_REASON_PHRASE, ?HYBI_REASON_VAL},
		{?HYBI_UPGRADE,    ?HYBI_UPG_VAL},
		{?HYBI_CONNECTION, ?HYBI_CON_VAL}
	],

	gen_response_2(Response, Request).
%------------------------------------------------------------------------------
gen_response_2(Response, Request) ->
	Accept = resolve_trial(Request),
	
	gen_response_3(Response ++ [{?HYBI_ACCEPT, Accept}], Request).
%------------------------------------------------------------------------------
gen_response_3(ResponsePartial, Request) ->
	Response = case header:find(?HYBI_PROTOCOL, Request) of
		{found, SubProtocolList} ->
			SubProtocol = 
				header:resolve_subprotocol(SubProtocolList),
			ResponsePartial ++ [{?HYBI_PROTOCOL, SubProtocol}];
		notfound ->
			ResponsePartial
	end,
	Response ++ [
		{undefined, []}, 
		{undefined, []}].
%------------------------------------------------------------------------------
