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

%------------------------------------------------------------------------------
parse_test() ->
	{WsRqResultH75, WsRqExpectedH75} = parse_request_test_ws_hixie75(),
	{WsRqResultH76, WsRqExpectedH76} = parse_request_test_ws_hixie76(),
	{WsRqResultHb7, WsRqExpectedHb7} = parse_request_test_ws_hybi07(),
	{WsRsResultH75, WsRsExpectedH75} = parse_response_test_ws_hixie75(),
	{WsRsResultH76, WsRsExpectedH76} = parse_response_test_ws_hixie76(),
	{WsRsResultHb7, WsRsExpectedHb7} = parse_response_test_ws_hybi7(),
	[
		?assertEqual(WsRqResultH75, WsRqExpectedH75),
		?assertEqual(WsRqResultH76, WsRqExpectedH76),
		?assertEqual(WsRqResultHb7, WsRqExpectedHb7),
		?assertEqual(WsRsResultH75, WsRsExpectedH75),
		?assertEqual(WsRsResultH76, WsRsExpectedH76),
		?assertEqual(WsRsResultHb7, WsRsExpectedHb7)
	].
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
parse_test_1(R, E) ->
	print(R, E),
	{R, E}.
%------------------------------------------------------------------------------
print(Result, Expected) ->
	io:format("resultado: ~p~nesperado: ~p~n~n", [Result, Expected]).
%------------------------------------------------------------------------------
parse_request_test_ws_hixie75() ->
	RequestSample = 
		"GET /demo HTTP/1.1\r\n" ++
		"Upgrade: WebSocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"Host: example.com\r\n" ++
		"Origin: http://example.com\r\n" ++
		"WebSocket-Protocol: sample\r\n\r\n",
	Expected = [
		{method, "GET"}, 
		{path, "/demo"}, 
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"}, 
		{"Host", "example.com"},
		{"Origin", "http://example.com"}, 
		{"WebSocket-Protocol", "sample"},
		{undefined, []}
	],
	Result = websocket_header:parse(RequestSample),
	parse_test_1(Result, Expected).
parse_request_test_ws_hixie76() ->
	RequestSample = 
		"GET /demo HTTP/1.1\r\n" ++
		"Host: example.com\r\n" ++
		"Connection: Upgrade\r\n" ++
		"Sec-WebSocket-Key2: 12998 5 Y3 1  .P00\r\n" ++
		"Sec-WebSocket-Protocol: sample\r\n" ++
		"Upgrade: WebSocket\r\n" ++
		"Sec-WebSocket-Key1: 4 @1  46546xW%0l 1 5\r\n" ++
		"Origin: http://example.com\r\n" ++
		"\r\n^:ds[4U",
	Expected = [
		{method, "GET"}, 
		{path, "/demo"}, 
		{"Host", "example.com"},
		{"Connection", "Upgrade"}, 
		{"Sec-WebSocket-Key2", "12998 5 Y3 1  .P00"},
		{"Sec-WebSocket-Protocol", "sample"},
		{"Upgrade", "WebSocket"},
		{"Sec-WebSocket-Key1", "4 @1  46546xW%0l 1 5"},
		{"Origin", "http://example.com"},
		{undefined, "^:ds[4U"}
	],
	Result = websocket_header:parse(RequestSample),
	parse_test_1(Result, Expected).
parse_request_test_ws_hybi07() ->
	RequestSample = 
		"GET /chat HTTP/1.1\r\n" ++
		"Host: server.example.com\r\n" ++
		"Upgrade: websocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"Sec-WebSocket-Key: dGhlIHNbXBsZSBub25jZQ==\r\n" ++
		"Sec-WebSocket-Origin: http://example.com\r\n" ++
		"Sec-WebSocket-Protocol: chat, superchat\r\n" ++
		"Sec-WebSocket-Version: 7\r\n\r\n",
	Expected = [
		{method, "GET"}, 
		{path, "/chat"}, 
		{"Host", "server.example.com"},
		{"Upgrade", "websocket"}, 
		{"Connection", "Upgrade"}, 
		{"Sec-WebSocket-Key", "dGhlIHNbXBsZSBub25jZQ=="},
		{"Sec-WebSocket-Origin", "http://example.com"},
		{"Sec-WebSocket-Protocol", "chat, superchat"},
		{"Sec-WebSocket-Version", "7"}, 
		{undefined, []}
	],
	Result = websocket_header:parse(RequestSample),
	parse_test_1(Result, Expected).
%------------------------------------------------------------------------------
parse_response_test_ws_hixie75() ->
	ResponseSample = 
		"HTTP/1.1 101 Web Socket Protocol Handshake\r\n" ++
		"Upgrade: WebSocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"WebSocket-Origin: http://example.com\r\n" ++
		"WebSocket-Location: ws://example.com/demo\r\n" ++
		"WebSocket-Protocol: sample\r\n\r\n",
	Expected = [
		{status, "101"},
		{reason, "Web Socket Protocol Handshake"},
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"},
		{"WebSocket-Origin", "http://example.com"},
		{"WebSocket-Location", "ws://example.com/demo"},
		{"WebSocket-Protocol", "sample"},
		{undefined, []}
		],
	Result = websocket_header:parse(ResponseSample),
	parse_test_1(Result, Expected).
parse_response_test_ws_hixie76() ->
	ResponseSample = 
		"HTTP/1.1 101 Web Socket Protocol Handshake\r\n" ++
		"Upgrade: WebSocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"WebSocket-Origin: http://example.com\r\n" ++
		"WebSocket-Location: ws://example.com/demo\r\n" ++
		"WebSocket-Protocol: sample\r\n\r\n" ++
		"8jKS'y:G*Co,Wxa-",
	Expected = [
		{status, "101"},
		{reason, "Web Socket Protocol Handshake"},
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"},
		{"WebSocket-Origin", "http://example.com"},
		{"WebSocket-Location", "ws://example.com/demo"},
		{"WebSocket-Protocol", "sample"},
		{undefined, "8jKS'y:G*Co,Wxa-"}
		],
	Result = websocket_header:parse(ResponseSample),
	parse_test_1(Result, Expected).
parse_response_test_ws_hybi7() ->
	ResponseSample = 
		"HTTP/1.1 101 Switching Protocols\r\n" ++
		"Upgrade: websocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n" ++
		"Sec-WebSocket-Protocol: chat\r\n\r\n",
	Expected = [
		{status, "101"},
		{reason, "Switching Protocols"},
		{"Upgrade", "websocket"},
		{"Connection", "Upgrade"},
		{"Sec-WebSocket-Accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="},
		{"Sec-WebSocket-Protocol", "chat"},
		{undefined, []}
		],
	Result = websocket_header:parse(ResponseSample),
	parse_test_1(Result, Expected).
