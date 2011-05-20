%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo de teste unitario de WebSocket Header
%% Criado: 05/18/11 22:40:03 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(websocket_header_tests).
-author("elmiliox@gmail.com").
-vsn(1).

-include_lib("eunit/include/eunit.hrl").

parse_request_test() ->
	WebSocketRequest = 
		"GET /chat HTTP/1.1\r\n" ++
		"Host: server.example.com\r\n" ++
		"Upgrade: websocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"Sec-WebSocket-Key: dGhlIHNbXBsZSBub25jZQ==\r\n" ++
		"Sec-WebSocket-Origin: http://example.com\r\n" ++
		"Sec-WebSocket-Protocol: chat, superchat\r\n" ++
		"Sec-WebSocket-Version: 7\r\n\r\n",
	WsExpected = [
		{method, "GET"}, {path, "/chat"}, {"Host", "server.example.com"},
		{"Upgrade", "websocket"}, {"Connection", "Upgrade"}, 
		{"Sec-WebSocket-Key", "dGhlIHNbXBsZSBub25jZQ=="},
		{"Sec-WebSocket-Origin", "http://example.com"},
		{"Sec-WebSocket-Protocol", "chat, superchat"},
		{"Sec-WebSocket-Version", "7"}, {undefined, []}
	],
	WsResult = websocket_header:parse_request(WebSocketRequest),
	io:format("~p", [WsResult]),
	?assertEqual(WsResult, WsExpected).
