%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo de teste unitario de WebSocket Header
%% Criado: 05/18/11 22:40:03 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011
%------------------------------------------------------------------------------
-module(wslib.header_tests).
-author("elmiliox@gmail.com").
-vsn(1).
%------------------------------------------------------------------------------
%------------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("ws_protocol_header.hrl").
%------------------------------------------------------------------------------
-import(io).
%------------------------------------------------------------------------------
parse_test() ->
	{ReqResultH75, ReqExpectH75} = parse_test_1(
		hx75_req(), hx75_req_fmt()),
	{ReqResultH76, ReqExpectH76} = parse_test_1(
		hx76_req(), hx76_req_fmt()),
	{ReqResultHb7, ReqExpectHb7} = parse_test_1(
		hb07_req(), hb07_req_fmt()),
	{ResResultH75, ResExpectH75} = parse_test_1(
		hx75_res(), hx75_res_fmt()),
	{ResResultH76, ResExpectH76} = parse_test_1(
		hx76_res(), hx76_res_fmt()),
	{ResResultHb7, ResExpectHb7} = parse_test_1(
		hb07_res(), hb07_res_fmt()),

	[
		?assertEqual(ReqResultH75, ReqExpectH75),
		?assertEqual(ReqResultH76, ReqExpectH76),
		?assertEqual(ReqResultHb7, ReqExpectHb7),
		?assertEqual(ResResultH75, ResExpectH75),
		?assertEqual(ResResultH76, ResExpectH76),
		?assertEqual(ResResultHb7, ResExpectHb7),

		?assertError(badarg, header:parse(atom)),
		?assertError(badarg, header:parse({})),
		?assertError(badarg, header:parse(1337))
	].
%------------------------------------------------------------------------------
find_test() ->
	Find = fun(K, L) -> header:find(K, L) end,

	H75Q = hx75_req_fmt(),
	H75R = hx75_res_fmt(),

	H76Q = hx76_req_fmt(),
	H76R = hx76_res_fmt(),

	HB7Q = hb07_req_fmt(),
	HB7R = hb07_res_fmt(),

	[
		?assertEqual({found, "GET"},                Find(?HIXIE75_METHOD, H75Q)     ),
		?assertEqual({found, "/demo"},              Find(?HIXIE75_URI, H75Q)        ),
		?assertEqual({found, ?HIXIE75_CON_VAL},     Find(?HIXIE75_CONNECTION, H75Q) ),
		?assertEqual({found, ?HIXIE75_UPG_VAL},     Find(?HIXIE75_UPGRADE, H75Q)    ),
		?assertEqual({found, "example.com"},        Find(?HIXIE75_HOST, H75Q)       ),
		?assertEqual({found, "http://example.com"}, Find(?HIXIE75_ORIGIN_REQ, H75Q) ),
		?assertEqual({found, "sample"},             Find(?HIXIE75_PROTOCOL, H75Q)   ),
		?assertEqual({found, []},                   Find(undefined, H75Q)           ),

		?assertEqual(notfound, Find(?HIXIE75_STATUS_CODE, H75Q)   ),
		?assertEqual(notfound, Find(?HIXIE75_REASON_PHRASE, H75Q) ),
		?assertEqual(notfound, Find(?HIXIE75_ORIGIN_RES, H75Q)    ),
		?assertEqual(notfound, Find(?HIXIE75_LOCATION, H75Q)      )

	] ++ [
		?assertEqual({found, "101"},                Find(?HIXIE75_STATUS_CODE, H75R)   ),
		?assertEqual({found, ?HIXIE75_REASON_VAL},  Find(?HIXIE75_REASON_PHRASE, H75R) ),
		?assertEqual({found, ?HIXIE75_CON_VAL},     Find(?HIXIE75_CONNECTION, H75R) ),
		?assertEqual({found, ?HIXIE75_UPG_VAL},     Find(?HIXIE75_UPGRADE, H75R)    ),
		?assertEqual({found, "http://example.com"}, Find(?HIXIE75_ORIGIN_RES, H75R) ),
		?assertEqual({found, "ws://example.com/demo"}, Find(?HIXIE75_LOCATION, H75R)),
		?assertEqual({found, "sample"},             Find(?HIXIE75_PROTOCOL, H75R)   ),
		?assertEqual({found, []},                   Find(undefined, H75R)           ),

		?assertEqual(notfound, Find(?HIXIE75_METHOD, H75R) ),
		?assertEqual(notfound, Find(?HIXIE75_URI,    H75R) ),
		?assertEqual(notfound, Find(?HIXIE75_HOST,   H75R) ),
		?assertEqual(notfound, Find(?HIXIE75_ORIGIN_REQ, H75R) )

	] ++
	[
		?assertEqual({found, "GET"},                Find(?HIXIE76_METHOD, H76Q)     ),
		?assertEqual({found, "/demo"},              Find(?HIXIE76_URI,   H76Q)      ),
		?assertEqual({found, ?HIXIE76_CON_VAL},     Find(?HIXIE76_CONNECTION, H76Q) ),
		?assertEqual({found, ?HIXIE76_UPG_VAL},     Find(?HIXIE76_UPGRADE, H76Q)    ),
		?assertEqual({found, "example.com"},        Find(?HIXIE76_HOST,    H76Q)    ),
		?assertEqual({found, "http://example.com"}, Find(?HIXIE76_ORIGIN_REQ, H76Q) ),
		?assertEqual({found, "sample"},             Find(?HIXIE76_PROTOCOL,   H76Q) ),
		?assertEqual({found, "4 @1  46546xW%0l 1 5"},Find(?HIXIE76_KEY1, H76Q)      ),
		?assertEqual({found, "12998 5 Y3 1  .P00"}, Find(?HIXIE76_KEY2,  H76Q)      ),
		?assertEqual({found, []},                   Find(undefined, H76Q)           ),

		?assertEqual(notfound, Find(?HIXIE76_STATUS_CODE,   H76Q) ),
		?assertEqual(notfound, Find(?HIXIE76_REASON_PHRASE, H76Q) ),
		?assertEqual(notfound, Find(?HIXIE76_ORIGIN_RES,    H76Q) ),
		?assertEqual(notfound, Find(?HIXIE76_LOCATION, H76Q)      )

	] ++ [
		?assertEqual({found, "101"},                Find(?HIXIE76_STATUS_CODE,   H76R) ),
		?assertEqual({found, ?HIXIE76_REASON_VAL},  Find(?HIXIE76_REASON_PHRASE, H76R) ),
		?assertEqual({found, ?HIXIE76_CON_VAL},     Find(?HIXIE76_CONNECTION,    H76R) ),
		?assertEqual({found, ?HIXIE76_UPG_VAL},     Find(?HIXIE76_UPGRADE,       H76R) ),
		?assertEqual({found, "http://example.com"}, Find(?HIXIE76_ORIGIN_RES,    H76R) ),
		?assertEqual({found, "ws://example.com/demo"}, Find(?HIXIE76_LOCATION,   H76R) ),
		?assertEqual({found, "sample"},             Find(?HIXIE76_PROTOCOL,      H76R) ),
		?assertEqual({found, []},                   Find(undefined, H75R) ),

		?assertEqual(notfound, Find(?HIXIE75_METHOD, H76R) ),
		?assertEqual(notfound, Find(?HIXIE75_URI,    H76R) ),
		?assertEqual(notfound, Find(?HIXIE75_HOST,   H76R) ),
		?assertEqual(notfound, Find(?HIXIE75_ORIGIN_REQ, H76R) )
	] ++
	[
		?assertEqual({found, "GET"},                Find(?HYBI_METHOD, HB7Q)     ),
		?assertEqual({found, "/chat"},              Find(?HYBI_URI,    HB7Q)     ),
		?assertEqual({found, ?HYBI_CON_VAL},        Find(?HYBI_CONNECTION, HB7Q) ),
		?assertEqual({found, ?HYBI_UPG_VAL},        Find(?HYBI_UPGRADE,    HB7Q) ),
		?assertEqual({found, "server.example.com"}, Find(?HYBI_HOST,       HB7Q) ),
		?assertEqual({found, "http://example.com"}, Find(?HYBI_ORIGIN,     HB7Q) ),
		?assertEqual({found, "chat, superchat"},    Find(?HYBI_PROTOCOL,   HB7Q) ),
		?assertEqual({found, "dGhlIHNhbXBsZSBub25jZQ=="}, Find(?HYBI_KEY,   HB7Q) ),
		?assertEqual({found, "7"}, Find(?HYBI_VERSION, HB7Q) ),
		?assertEqual({found, []},  Find(undefined,     HB7Q) ),

		?assertEqual(notfound, Find(?HYBI_STATUS_CODE,   HB7Q) ),
		?assertEqual(notfound, Find(?HYBI_REASON_PHRASE, HB7Q) ),
		?assertEqual(notfound, Find(?HYBI_ACCEPT,    HB7Q) )
	] ++ [
		?assertEqual({found, "101"},             Find(?HYBI_STATUS_CODE,   HB7R) ),
		?assertEqual({found, ?HYBI_REASON_VAL},  Find(?HYBI_REASON_PHRASE, HB7R) ),
		?assertEqual({found, ?HYBI_CON_VAL},     Find(?HYBI_CONNECTION,    HB7R) ),
		?assertEqual({found, ?HYBI_UPG_VAL},     Find(?HYBI_UPGRADE,       HB7R) ),
		?assertEqual({found, "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}, Find(?HYBI_ACCEPT, HB7R) ),
		?assertEqual({found, "chat"},            Find(?HYBI_PROTOCOL, HB7R) ),
		?assertEqual({found, []},                Find(undefined,      HB7R) ),

		?assertEqual(notfound, Find(?HYBI_METHOD, HB7R) ),
		?assertEqual(notfound, Find(?HYBI_URI,    HB7R) ),
		?assertEqual(notfound, Find(?HYBI_HOST,   HB7R) ),
		?assertEqual(notfound, Find(?HYBI_ORIGIN, HB7R) ),
		?assertEqual(notfound, Find(?HYBI_KEY,    HB7R) ),
		?assertEqual(notfound, Find(?HYBI_VERSION,HB7R) )
	].
%------------------------------------------------------------------------------
type_test() ->
	[
		?assertEqual({ok, {?HIXIE75, response}}, 
			header:type(hx75_res_fmt())),
		?assertEqual({ok, {?HIXIE76, response}}, 
			header:type(hx76_res_fmt())),
		?assertEqual({ok, {?HYBI, response}}, 
			header:type(hb07_res_fmt())),
		?assertEqual({ok, {?HIXIE75, request}}, 
			header:type(hx75_req_fmt())),
		?assertEqual({ok, {?HIXIE76, request}},
		       	header:type(hx76_req_fmt())),
		?assertEqual({ok, {?HYBI, request}}, 
			header:type(hb07_req_fmt()))
	].
%------------------------------------------------------------------------------
to_string_test() ->
	[
		?assertEqual(hx75_req(), 
			to_string_test_1(hx75_req_fmt() , hx75_req()) ),
		?assertEqual(hx75_res(),
			to_string_test_1(hx75_res_fmt() , hx75_res()) ),

		?assertEqual(hx76_req(),
			to_string_test_1(hx76_req_fmt() , hx76_req()) ),
		?assertEqual(hx76_res(), 
			to_string_test_1(hx76_res_fmt() , hx76_res()) ),

		?assertEqual(hb07_req(),
			to_string_test_1(hb07_req_fmt() , hb07_req()) ),
		?assertEqual(hb07_res(),
			to_string_test_1(hb07_res_fmt() , hb07_res()) )
	].
%------------------------------------------------------------------------------
to_string_test_1(Sample, E) ->
	R = header:to_string(Sample),
	io:format("entrada:~p~nresultado: ~n"++R++"~nesperado: ~n"++E++"~n", 
		[Sample]),
	E.
%------------------------------------------------------------------------------
parse_test_1(Sample, E) ->
	R = header:parse(Sample),
	print(Sample, R, E),
	{R, E}.
%------------------------------------------------------------------------------
print(Input, Result, Expected) ->
	io:format("entrada:~n"++Input++"~nresultado: ~p~nesperado: ~p~n~n", 
		[Result, Expected]).
%------------------------------------------------------------------------------
hx75_req() ->
	"GET /demo HTTP/1.1\r\n" ++
	"Upgrade: WebSocket\r\n" ++
	"Connection: Upgrade\r\n" ++
	"Host: example.com\r\n" ++
	"Origin: http://example.com\r\n" ++
	"WebSocket-Protocol: sample\r\n" ++
	"\r\n".
hx75_res() ->
	"HTTP/1.1 101 Web Socket Protocol Handshake\r\n" ++
	"Upgrade: WebSocket\r\n" ++
	"Connection: Upgrade\r\n" ++
	"WebSocket-Origin: http://example.com\r\n" ++
	"WebSocket-Location: ws://example.com/demo\r\n" ++
	"WebSocket-Protocol: sample\r\n" ++
	"\r\n".
%------------------------------------------------------------------------------

hx76_req() ->
	"GET /demo HTTP/1.1\r\n" ++
	"Host: example.com\r\n" ++
	"Connection: Upgrade\r\n" ++
	"Sec-WebSocket-Key2: 12998 5 Y3 1  .P00\r\n" ++
	"Sec-WebSocket-Protocol: sample\r\n" ++
	"Upgrade: WebSocket\r\n" ++
	"Sec-WebSocket-Key1: 4 @1  46546xW%0l 1 5\r\n" ++
	"Origin: http://example.com\r\n" ++
	"\r\n^n:ds[4U".
hx76_res() ->
	"HTTP/1.1 101 WebSocket Protocol Handshake\r\n" ++
	"Upgrade: WebSocket\r\n" ++
	"Connection: Upgrade\r\n" ++
	"Sec-WebSocket-Origin: http://example.com\r\n" ++
	"Sec-WebSocket-Location: ws://example.com/demo\r\n" ++
	"Sec-WebSocket-Protocol: sample\r\n" ++
	"\r\n8jKS'y:G*Co,Wxa-".
%------------------------------------------------------------------------------
hb07_req() ->
	"GET /chat HTTP/1.1\r\n" ++
	"Host: server.example.com\r\n" ++
	"Upgrade: websocket\r\n" ++
	"Connection: Upgrade\r\n" ++
	"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n" ++
	"Sec-WebSocket-Origin: http://example.com\r\n" ++
	"Sec-WebSocket-Protocol: chat, superchat\r\n" ++
	"Sec-WebSocket-Version: 7\r\n" ++ 
	"\r\n".
hb07_res() ->
	"HTTP/1.1 101 Switching Protocols\r\n" ++
	"Upgrade: websocket\r\n" ++
	"Connection: Upgrade\r\n" ++
	"Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=\r\n" ++
	"Sec-WebSocket-Protocol: chat\r\n" ++ 
	"\r\n".
%------------------------------------------------------------------------------
hx75_req_fmt() ->
	[
		{method, "GET"}, 
		{path, "/demo"}, 
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"}, 
		{"Host", "example.com"},
		{"Origin", "http://example.com"}, 
		{"WebSocket-Protocol", "sample"},
		{undefined, []},
		{undefined, []}
	].
hx75_res_fmt() ->
	[
		{status, "101"},
		{reason, "Web Socket Protocol Handshake"},
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"},
		{"WebSocket-Origin", "http://example.com"},
		{"WebSocket-Location", "ws://example.com/demo"},
		{"WebSocket-Protocol", "sample"},
		{undefined, []},
		{undefined, []}
	].
%------------------------------------------------------------------------------
hx76_req_fmt() ->
	[
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
		{undefined, "^n:ds[4U"}
	].
hx76_res_fmt() ->
	[
		{status, "101"},
		{reason, "WebSocket Protocol Handshake"},
		{"Upgrade", "WebSocket"},
		{"Connection", "Upgrade"},
		{"Sec-WebSocket-Origin", "http://example.com"},
		{"Sec-WebSocket-Location", "ws://example.com/demo"},
		{"Sec-WebSocket-Protocol", "sample"},
		{undefined, []},
		{undefined, "8jKS'y:G*Co,Wxa-"}
	].
%------------------------------------------------------------------------------
hb07_req_fmt() ->
	[
		{method, "GET"}, 
		{path, "/chat"}, 
		{"Host", "server.example.com"},
		{"Upgrade", "websocket"}, 
		{"Connection", "Upgrade"}, 
		{"Sec-WebSocket-Key", "dGhlIHNhbXBsZSBub25jZQ=="},
		{"Sec-WebSocket-Origin", "http://example.com"},
		{"Sec-WebSocket-Protocol", "chat, superchat"},
		{"Sec-WebSocket-Version", "7"}, 
		{undefined, []},
		{undefined, []}
	].
hb07_res_fmt() ->
	[
		{status, "101"},
		{reason, "Switching Protocols"},
		{"Upgrade", "websocket"},
		{"Connection", "Upgrade"},
		{"Sec-WebSocket-Accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="},
		{"Sec-WebSocket-Protocol", "chat"},
		{undefined, []},
		{undefined, []}
	].
