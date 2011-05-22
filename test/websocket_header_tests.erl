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
	{ReqResultH75, ReqExpectH75} = parse_request_hixie75(),
	{ReqResultH76, ReqExpectH76} = parse_request_hixie76(),
	{ReqResultHb7, ReqExpectHb7} = parse_request_hybi_07(),
	{ResResultH75, ResExpectH75} = parse_response_hixie75(),
	{ResResultH76, ResExpectH76} = parse_response_hixie76(),
	{ResResultHb7, ResExpectHb7} = parse_response_hybi_07(),
	[
		?assertEqual(ReqResultH75, ReqExpectH75),
		?assertEqual(ReqResultH76, ReqExpectH76),
		?assertEqual(ReqResultHb7, ReqExpectHb7),
		?assertEqual(ResResultH75, ResExpectH75),
		?assertEqual(ResResultH76, ResExpectH76),
		?assertEqual(ResResultHb7, ResExpectHb7),
		?assertError(badarg, websocket_header:parse(atom)),
		?assertError(badarg, websocket_header:parse(1337))
	].
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
parse_test_1(Sample, E) ->
	R = websocket_header:parse(Sample),
	print(R, E),
	{R, E}.
%------------------------------------------------------------------------------
print(Result, Expected) ->
	io:format("resultado: ~p~nesperado: ~p~n~n", [Result, Expected]).
%------------------------------------------------------------------------------
parse_request_hixie75() ->
	RequestSample = 
		"GET /demo HTTP/1.1\r\n" ++
		"Upgrade: WebSocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"Host: example.com\r\n" ++
		"Origin: http://example.com\r\n" ++
		"WebSocket-Protocol: sample\r\n" ++
		"\r\n",
	Expected = [
		{method, "GET"}, 
		{path, "/demo"}, 
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"}, 
		{"Host", "example.com"},
		{"Origin", "http://example.com"}, 
		{"WebSocket-Protocol", "sample"},
		{undefined, []},
		{undefined, []}
	],
	parse_test_1(RequestSample, Expected).
parse_response_hixie75() ->
	ResponseSample = 
		"HTTP/1.1 101 Web Socket Protocol Handshake\r\n" ++
		"Upgrade: WebSocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"WebSocket-Origin: http://example.com\r\n" ++
		"WebSocket-Location: ws://example.com/demo\r\n" ++
		"WebSocket-Protocol: sample\r\n" ++
		"\r\n",
	Expected = [
		{status, "101"},
		{reason, "Web Socket Protocol Handshake"},
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"},
		{"WebSocket-Origin", "http://example.com"},
		{"WebSocket-Location", "ws://example.com/demo"},
		{"WebSocket-Protocol", "sample"},
		{undefined, []},
		{undefined, []}
		],
	parse_test_1(ResponseSample, Expected).
%------------------------------------------------------------------------------
parse_request_hixie76() ->
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
		{undefined, []},
		{undefined, "^:ds[4U"}
	],
	parse_test_1(RequestSample, Expected).
parse_response_hixie76() ->
	ResponseSample = 
		"HTTP/1.1 101 Web Socket Protocol Handshake\r\n" ++
		"Upgrade: WebSocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"WebSocket-Origin: http://example.com\r\n" ++
		"WebSocket-Location: ws://example.com/demo\r\n" ++
		"WebSocket-Protocol: sample\r\n" ++
		"\r\n8jKS'y:G*Co,Wxa-",
	Expected = [
		{status, "101"},
		{reason, "Web Socket Protocol Handshake"},
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"},
		{"WebSocket-Origin", "http://example.com"},
		{"WebSocket-Location", "ws://example.com/demo"},
		{"WebSocket-Protocol", "sample"},
		{undefined, []},
		{undefined, "8jKS'y:G*Co,Wxa-"}
		],
	parse_test_1(ResponseSample, Expected).
%------------------------------------------------------------------------------
parse_request_hybi_07() ->
	RequestSample = 
		"GET /chat HTTP/1.1\r\n" ++
		"Host: server.example.com\r\n" ++
		"Upgrade: websocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"Sec-WebSocket-Key: dGhlIHNbXBsZSBub25jZQ==\r\n" ++
		"Sec-WebSocket-Origin: http://example.com\r\n" ++
		"Sec-WebSocket-Protocol: chat, superchat\r\n" ++
		"Sec-WebSocket-Version: 7\r\n" ++ 
		"\r\n",
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
		{undefined, []},
		{undefined, []}
	],
	parse_test_1(RequestSample, Expected).
parse_response_hybi_07() ->
	ResponseSample = 
		"HTTP/1.1 101 Switching Protocols\r\n" ++
		"Upgrade: websocket\r\n" ++
		"Connection: Upgrade\r\n" ++
		"Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n" ++
		"Sec-WebSocket-Protocol: chat\r\n" ++ 
		"\r\n",
	Expected = [
		{status, "101"},
		{reason, "Switching Protocols"},
		{"Upgrade", "websocket"},
		{"Connection", "Upgrade"},
		{"Sec-WebSocket-Accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="},
		{"Sec-WebSocket-Protocol", "chat"},
		{undefined, []},
		{undefined, []}
		],
	parse_test_1(ResponseSample, Expected).
