%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Module de teste unitario de gen_ws
%% Criado: 05/17/11 20:35:56 (HPC)
%% @copyright Emiliano@2011

-module(gen_ws_tests).
-author("elmiliox@gmail.com").
-vsn(1).

-include_lib("eunit/include/eunit.hrl").
-define(TODO, {error, todo}).
%------------------------------------------------------------------------------
connect_test() ->
	_Url = "ws://echo.websocket.org/",
	_SecUrl = "wss://echo.websocket.org",
	_Origin = {origin, "http://websocket.org"},

	[?_assert(gen_ws:connect("") == ?TODO)].
listen_test() ->
	[?_assert(gen_ws:listen(0, []) == ?TODO)].
%------------------------------------------------------------------------------
accept_test() ->
	[?_assert(gen_ws:accept(null) == ?TODO)].

recv_test() ->
	[?_assert(gen_ws:recv(null) == ?TODO)].

send_test() ->
	[?_assert(gen_ws:send(null, {undefined, ""}) == ?TODO)].

close_test() ->
	[?_assert(gen_ws:close(null, {undefined, ""}) == ?TODO)].
%------------------------------------------------------------------------------
%% Obtem a Url utilizada para estabelecer a Conexao WebSocket
geturl_test() ->
	[?_assert(gen_ws:geturl(null) == ?TODO)].

getsubprotocol_test() ->
	[?_assert(gen_ws:getsubprotocol(null) == ?TODO)].

getstate_test() ->
	[?_assert(gen_ws:getstate(null) == ?TODO)].
%------------------------------------------------------------------------------
