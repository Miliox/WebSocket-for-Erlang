%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Teste de hybi07_lib
%% Criado: 06/05/11 19:11:21 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

%------------------------------------------------------------------------------
-module(wslib.hybi07_tests).
-author("elmiliox@gmail.com").
-vsn(1).
%------------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%------------------------------------------------------------------------------
-import(base64).
%------------------------------------------------------------------------------
resolve_trial_test() ->
	Request = header_tests:hb07_req_fmt(),
	Answer = "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=",
	
	[?assertEqual(Answer, hybi07:resolve_trial(Request))].
%------------------------------------------------------------------------------
make_trial_test() ->
	{K, A} = hybi07:make_trial(),
	[
		?assertEqual(16, size(base64:decode(K) ) ),
		?assertEqual(A, hybi07:resolve_trial_from_key(K))
	].
%------------------------------------------------------------------------------
