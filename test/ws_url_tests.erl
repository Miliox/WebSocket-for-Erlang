%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 06/05/11 20:21:57 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(ws_url_tests).
-author("elmiliox@gmail.com").
-vsn(1).

%------------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%------------------------------------------------------------------------------
-define(URL1, "ws://www.google.com/").
-define(URL2, "wss://echo.websocket.org").
-define(URL3, "ws://test.ws.com:8080/path").
-define(URL4, "wss://secure.ws.org:443").
%------------------------------------------------------------------------------
-define(URL1_FMT, {normal, "www.google.com", "www.google.com", 80, "/"}).
-define(URL2_FMT, {secure, "echo.websocket.org", "echo.websocket.org", 443, "/"}).
-define(URL3_FMT, {normal, "test.ws.com", "test.ws.com:8080", 8080, "/path"}).
-define(URL4_FMT, {secure, "secure.ws.org", "secure.ws.org:443", 443, "/"}).
%------------------------------------------------------------------------------
scheme_test() ->
	[
		?assertEqual("ws", ws_url:scheme(normal)),
		?assertEqual("wss", ws_url:scheme(secure)),
		?assertError(badarg, ws_url:scheme(other))
	].
%------------------------------------------------------------------------------
parse_test() ->
	[
		?assertEqual(?URL1_FMT, ws_url:parse(?URL1)),
		?assertEqual(?URL2_FMT, ws_url:parse(?URL2)),
		?assertEqual(?URL3_FMT, ws_url:parse(?URL3)),
		?assertEqual(?URL4_FMT, ws_url:parse(?URL4))
	].
%------------------------------------------------------------------------------
to_string_test() ->
	[
		?assertEqual(?URL1, ws_url:to_string(?URL1_FMT)),
		?assertEqual(?URL2++"/", ws_url:to_string(?URL2_FMT)),
		?assertEqual(?URL3, ws_url:to_string(?URL3_FMT)),
		?assertEqual(?URL4++"/", ws_url:to_string(?URL4_FMT))
	].
%------------------------------------------------------------------------------
