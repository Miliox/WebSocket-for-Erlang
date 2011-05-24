%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 05/23/11 13:34:44 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(ws_hixie76_lib_tests).
-author("elmiliox@gmail.com").
-vsn(1).

-include_lib("eunit/include/eunit.hrl").
-include("ws_protocol_header.hrl").

%------------------------------------------------------------------------------
gen_response_test() ->
[
	?assertEqual(
		ws_header_tests:hx76_res_fmt(), 
		ws_hixie76_lib:gen_response(
			ws_header_tests:hx76_req_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		ws_hixie76_lib:gen_response(
			ws_header_tests:hx76_res_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		ws_hixie76_lib:gen_response(
			ws_header_tests:hx75_req_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		ws_hixie76_lib:gen_response(
			ws_header_tests:hx75_res_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		ws_hixie76_lib:gen_response(
			ws_header_tests:hb07_req_fmt()
		)),
	?assertEqual(
		{error, invalid_request}, 
		ws_hixie76_lib:gen_response(
			ws_header_tests:hb07_res_fmt()
		))
].
%------------------------------------------------------------------------------
make_trial_test() ->
	Trial = ws_hixie76_lib:make_trial(),
	{K1, K2, K3, Solution} = Trial,
	Answer = ws_hixie76_lib:resolve_trial({K1,K2,K3}),

	[
		?assertEqual(Solution, Answer),
		?assertEqual(8, length(K3) ),
		?assertEqual(16,length(Solution) )].
%------------------------------------------------------------------------------
resolve_trial_test() ->
	K1_1 = "4 @1  46546xW%0l 1 5",
	K2_1 = "12998 5 Y3 1  .P00",
	K3_1 = "^n:ds[4U",
	Solution_1 = "8jKS'y:G*Co,Wxa-",
	Answer_1 = ws_hixie76_lib:resolve_trial({K1_1,K2_1,K3_1}),
	
	K1_2 = "18x 6]8vM;54 *(5:  {   U1]8  z [  8",
	K2_2 = "1_ tx7X d  <  nw  334J702) 7]o}` 0",
	K3_2 = "Tm[K T2u",
	Solution_2 = "fQJ,fN/4F4!~K~MH",
	Answer_2 = ws_hixie76_lib:resolve_trial({K1_2,K2_2,K3_2}),

	[ 
		?assertEqual(Solution_1, Answer_1), 
		?assertEqual(Solution_2, Answer_2) ].
%------------------------------------------------------------------------------
decode_key_test() ->
	K1 = "4 @1  46546xW%0l 1 5",
	K2 = "12998 5 Y3 1  .P00",
	K3 = "18x 6]8vM;54 *(5:  {   U1]8  z [  8",
	K4 = "1_ tx7X d  <  nw  334J702) 7]o}` 0",

	[
		?assertEqual(trunc(4146546015 / 5), 
			ws_hixie76_lib:decode_key(K1)),
		?assertEqual(trunc(1299853100 / 5), 
			ws_hixie76_lib:decode_key(K2)),
		?assertEqual(trunc(1868545188 / 12), 
			ws_hixie76_lib:decode_key(K3)),
		?assertEqual(trunc(1733470270 / 10), 
			ws_hixie76_lib:decode_key(K4))
	].
%------------------------------------------------------------------------------
encode_key_test() ->
	% Draft-Hixie76-Example
	K1 = 155712099,
	S1 = 12,
	EncKey1 = ws_hixie76_lib:encode_key(K1, S1),
	io:format("{~p}", [EncKey1]),

	K2 = 173347027,
	S2 = 10,
	EncKey2 = ws_hixie76_lib:encode_key(K2, S2),

	[ 
		?assertEqual(K1, ws_hixie76_lib:decode_key(EncKey1)),
		?assertEqual(K2, ws_hixie76_lib:decode_key(EncKey2))
	].
%------------------------------------------------------------------------------
