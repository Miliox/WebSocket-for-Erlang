%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 05/23/11 13:34:44 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(hixie76_lib_tests).
-author("elmiliox@gmail.com").
-vsn(1).

-include_lib("eunit/include/eunit.hrl").
-include("ws_protocol_header.hrl").

%------------------------------------------------------------------------------
gen_request_test() ->
	Url = "ws://echo.websocket.org/test",
	Origin = "http://www.websocket.org",
	SubProt = ["echo", "superecho"],

	{Request, Validate} = hixie76_lib:gen_request(Url, Origin, SubProt),
	Response = hixie76_lib:gen_response(Request),

	% ?debugMsg("request:~n"),
	% ?debugFmt("~p",[Request]),
	% ?debugMsg("response:~n"),
	% ?debugFmt("~p",[Response]),

	RespExp = [
		{status, "101"},
		{reason, "WebSocket Protocol Handshake"},
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"},
		{"Sec-WebSocket-Origin", Origin},
		{"Sec-WebSocket-Location", Url},
		{"Sec-WebSocket-Protocol", "echo"},
		{undefined, []},
		{undefined, Validate}
	],

	[ ?assertEqual(RespExp, Response) ].
%------------------------------------------------------------------------------
gen_response_test() ->
[
	?assertEqual(
		ws_header_tests:hx76_res_fmt(), 
		hixie76_lib:gen_response(
			ws_header_tests:hx76_req_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		hixie76_lib:gen_response(
			ws_header_tests:hx76_res_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		hixie76_lib:gen_response(
			ws_header_tests:hx75_req_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		hixie76_lib:gen_response(
			ws_header_tests:hx75_res_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		hixie76_lib:gen_response(
			ws_header_tests:hb07_req_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		hixie76_lib:gen_response(
			ws_header_tests:hb07_res_fmt()
		))
].
%------------------------------------------------------------------------------
make_trial_test() ->
	Trial = hixie76_lib:make_trial(),
	{K1, K2, K3, Solution} = Trial,
	Answer = hixie76_lib:resolve_trial({K1,K2,K3}),

	[
		?assertEqual(Solution, Answer),
		?assertEqual(8, length(K3) ),
		?assertEqual(16,length(Solution) )].
%------------------------------------------------------------------------------
resolve_trial_test() ->
	Request = ws_header_tests:hx76_req_fmt(),
	Answer = hixie76_lib:resolve_trial(Request),
	% Draft-Hixie76-Example1
	K1_1 = "4 @1  46546xW%0l 1 5",
	K2_1 = "12998 5 Y3 1  .P00",
	K3_1 = "^n:ds[4U",
	Answer_1 = hixie76_lib:resolve_trial({K1_1,K2_1,K3_1}),

	Solution_1 = "8jKS'y:G*Co,Wxa-",
	% Draft-Hixie76-Example2
	K1_2 = "18x 6]8vM;54 *(5:  {   U1]8  z [  8",
	K2_2 = "1_ tx7X d  <  nw  334J702) 7]o}` 0",
	K3_2 = "Tm[K T2u",
	Solution_2 = "fQJ,fN/4F4!~K~MH",
	Answer_2 = hixie76_lib:resolve_trial({K1_2,K2_2,K3_2}),

	[ 
		?assertEqual(Solution_1, Answer), 
		?assertEqual(Solution_1, Answer_1), 
		?assertEqual(Solution_2, Answer_2)
	].
%------------------------------------------------------------------------------
decode_key_test() ->
	% Draft-Hixie76-Example1
	K1 = "4 @1  46546xW%0l 1 5", Key1 = trunc(4146546015 / 5), 
	K2 = "12998 5 Y3 1  .P00",   Key2 = trunc(1299853100 / 5), 
	% Draft-Hixie76-Example2
	K3 = "18x 6]8vM;54 *(5:  {   U1]8  z [  8", Key3 = trunc(1868545188 / 12), 
	K4 = "1_ tx7X d  <  nw  334J702) 7]o}` 0",  Key4 = trunc(1733470270 / 10), 

	[
		?assertEqual(Key1, hixie76_lib:decode_key(K1)),
		?assertEqual(Key2, hixie76_lib:decode_key(K2)),
		?assertEqual(Key3, hixie76_lib:decode_key(K3)),
		?assertEqual(Key4, hixie76_lib:decode_key(K4))
	].
%------------------------------------------------------------------------------
encode_key_test() ->
	% Draft-Hixie76-Example1
	Key1 = 155712099, Sp1 = 12,
	EncKey1 = hixie76_lib:encode_key(Key1, Sp1),
	% Draft-Hixie76-Example2
	Key2 = 173347027, Sp2 = 10,
	EncKey2 = hixie76_lib:encode_key(Key2, Sp2),

	[ 
		?assertEqual(Key1, hixie76_lib:decode_key(EncKey1)),
		?assertEqual(Key2, hixie76_lib:decode_key(EncKey2))
	].
%------------------------------------------------------------------------------
