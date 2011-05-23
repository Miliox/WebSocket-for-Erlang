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
