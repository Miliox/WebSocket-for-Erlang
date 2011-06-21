%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Gerar um WebSocket que comporte-se como os gerados pelo gen_tcp 
%%            ou gen_udp.
%% Criado: 05/17/11 20:00:39 (HPC)
%% Copyright Emiliano@2011

-module(gen_ws).
-author("elmiliox@gmail.com").
-vsn(3).
%------------------------------------------------------------------------------
-include("gen_ws.hrl").
-include("ws_frame.hrl").
%------------------------------------------------------------------------------
-import(ws_url).
-import(gen_tcp).
-import(hixie76_lib).
%------------------------------------------------------------------------------
-export([connect/1, connect/2, listen/2]).
-export([accept/1, accept/2, recv/1, recv/2, send/2, close/1]).
-export([geturl/1, getsubprotocol/1, getstate/1]).
%------------------------------------------------------------------------------
%% Cria um WebSocket a ser usado pelo Cliente
connect(Url) ->
	connect(Url, ?DEF_CON_OPT).
connect(Url, Options) ->
	{Mode, Address, _, Port, _} = ws_url:parse(Url),
	
	case Mode of
		normal -> normal_connect(Url, Address, Port, Options);
		secure -> {error, notsupport}
	end.
%------------------------------------------------------------------------------
get_opt(Key, Dict) ->
	case orddict:find(Key, Dict) of
		{ok, Value} -> 
			Value;
		error -> 
			get_default(Key)
	end.
%------------------------------------------------------------------------------
get_default(origin) -> ?DEF_ORIGIN;
get_default(timeout) -> ?DEF_TIMEOUT;
get_default(subprotocol) -> ?DEF_SUBP;
get_default(_) -> erlang:error(badarg).
%------------------------------------------------------------------------------
%% Cria um WebSocket a ser usado pelo Servidor
listen(Port) ->
	listen(Port, []).
listen(Port, _Options) ->
	ListenPid = spawn(?MODULE, listen_start, [Port, self()]),
	receive
		{ListenPid, ok} ->
			{ok, ?WSL_FMT(ListenPid)};
		{ListenPid, Reason} ->
			{error, Reason}
	end.
%------------------------------------------------------------------------------
%% Efetua o HandShake e estabelece uma Conexao WebSocket
accept(ListenWebSocket) ->
	accept(ListenWebSocket, infinity).
accept(?WSL_FMT(ListenPid), Timeout) ->
	ListenPid ! ?ACCEPT_REQ(Timeout),
	receive
		?ACCEPT_RES_OK(ListenPid, WebSocket) ->
			{ok, WebSocket};
		?ACCEPT_RES_ERROR(ListenPid, Reason) ->
			{error, Reason}
	end;
accept(_, _) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
%% Recebe uma mensagem transmitida via WebSocket
recv(WebSocket) ->
	recv(WebSocket, infinity).
recv(?WS_FMT(Handler), Timeout) ->
	Handler ! ?RECV_REQ(Timeout),
	receive
		?RECV_RES(Handler, Data) -> 
			{ok, Data}
		after 2000 ->
			{error, timeout}
	end.
%------------------------------------------------------------------------------
%% Envia uma mensagem via WebSocket
send(?WS_FMT(Handler), Data) when is_list(Data) ->
	Handler ! ?SEND_REQ(Data),
	receive
		?SEND_RES_OK(Handler) -> ok;
		?SEND_RES_ERROR(Handler, Reason) -> 
			{error, Reason}
		after 2000 ->
			{error, timeout}
	end;
send(WebSocket, Data) when is_binary(Data) ->
	send(WebSocket, binary_to_list(Data));
send(_, _) ->
	{error, einval}.
%------------------------------------------------------------------------------
%% Encerra uma conexao WebSocket
close(?WS_FMT(Handler)) ->
	Handler ! ?CLOSE_REQ,
	ok;
close(?WSL_FMT(Handler)) ->
	Handler ! ?CLOSE_REQ,
	ok.
%------------------------------------------------------------------------------
%% Obtem a Url utilizada para estabelecer a Conexao WebSocket
geturl(_WebSocket) ->
	?TODO.
%------------------------------------------------------------------------------
%% Obtem o Subprotocolo definido durante o HandShake
getsubprotocol(_WebSocket) ->
	?TODO.
%------------------------------------------------------------------------------
%% Obtem o estado atual do WebSocket
getstate(_WebSocket) ->
	?TODO.
%------------------------------------------------------------------------------
% Internal Functions
%------------------------------------------------------------------------------
normal_connect(Url, Address, Port, Options) ->
	Origin = get_opt(origin, Options),
	Timeout = get_opt(timeout, Options),
	SubProtocol = get_opt(subprotocol, Options),

	case gen_tcp:connect(Address, Port, ?TCP_OPT, Timeout) of
		{ok, TCPSocket} ->
			make_handshake(Url, Origin, SubProtocol, TCPSocket);
		Error ->
			Error
	end.
%------------------------------------------------------------------------------
make_handshake(Url, Origin, SubProtocol, TCPSocket) ->
	{Request, Answer} = hixie76_lib:gen_request(Url, Origin, SubProtocol),
	RequestHeader = ws_header:to_string(Request),

	HandlerParameter = [{url, Url}, {origin, Origin}, {request, Request}],

	case gen_tcp:send(TCPSocket, RequestHeader) of
		ok ->
			receive_response(TCPSocket, Answer, HandlerParameter);
		Error ->
			Error
	end.
%------------------------------------------------------------------------------
receive_response(TCPSocket, Answer, HandlerParameter) ->
	case catch(receive_response_1(TCPSocket, Answer, HandlerParameter)) of
		{'EXIT', {{case_clause, Error}, _}} ->
			Error;
		{'EXIT', {Reason, _}} ->
			{error, Reason};
		Sucess ->
			Sucess
	end.
%------------------------------------------------------------------------------
receive_response_1(TCPSocket, Answer, HandlerParameter) ->
	ResponseHeader = receive_header(TCPSocket),
	ServerSolution = receive_response_solution(TCPSocket),

	Response = ws_header:parse(ResponseHeader ++ ServerSolution),
	SubProtocol = hixie76_lib:get_subprotocol(Response),

	case ServerSolution == Answer of
		true ->
			WebSocket = create_websocket_handler(
				TCPSocket, 
				HandlerParameter ++ 
					[{response, Response}, 
					 {subprotocol, SubProtocol}]),
			 {ok, WebSocket};
		false ->
			?REPLY_ERROR
	end.	
%------------------------------------------------------------------------------
receive_header(TCPSocket) ->
	receive_header_loop(TCPSocket, []).
%------------------------------------------------------------------------------
% Header Loop Termina quanto encontrar CRLFCRLF
receive_header_loop(TCPSocket, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?CR} ->
			receive_header_loop_1(
				TCPSocket, ?CR ++ RevBuffer);
		{ok, Char} ->
			receive_header_loop(
				TCPSocket, Char ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
% Falta LFCRLF
receive_header_loop_1(TCPSocket, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?LF} ->
			receive_header_loop_2(
				TCPSocket, ?LF ++ RevBuffer);
		{ok, Char} ->
			receive_header_loop(
				TCPSocket, Char ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
% Falta CRLF
receive_header_loop_2(TCPSocket, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?CR} ->
			receive_header_loop_3(
				TCPSocket, ?CR ++ RevBuffer);
		{ok, Char} ->
			receive_header_loop(
				TCPSocket, Char ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
% Falta LF
receive_header_loop_3(TCPSocket, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?LF} ->
			lists:reverse(?LF ++ RevBuffer);
		{ok, Char} ->
			receive_header_loop(
				TCPSocket, Char ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
receive_response_solution(TCPSocket) ->
	receive_len(TCPSocket, ?SOLUTION_LEN, []).
%------------------------------------------------------------------------------
receive_len(_, Len, RevBuffer) when Len =< 0 ->
	lists:reverse(RevBuffer);
receive_len(TCPSocket, Len, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, Byte} -> receive_len(
				TCPSocket, Len-1, Byte ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
create_websocket_handler(TCPSocket, HandlerParameter) ->
	create_websocket_handler(TCPSocket, HandlerParameter, self()).
create_websocket_handler(TCPSocket, HandlerParameter, Owner) ->
	HandlerPid = spawn(?MODULE, main_start, [TCPSocket, Owner, HandlerParameter]),
	gen_tcp:controlling_process(TCPSocket, HandlerPid),

	WebSocket = ?WS_FMT(HandlerPid),

	WebSocket.
%------------------------------------------------------------------------------
main_start(TCPSocket, Owner, HandlerParameter) ->
	main_load(HandlerParameter),

	MailBox = queue:new(),
	Receiver = spawn_link(?MODULE, receiver_start, [TCPSocket, self()]),

	main_loop(TCPSocket, Owner, Receiver, MailBox).
%------------------------------------------------------------------------------
main_load([]) -> ok;
main_load([{Key, Value}|Parameter]) -> 
	put(Key, Value),
	main_load(Parameter);
main_load([_|Parameter]) ->
	main_load(Parameter).
%------------------------------------------------------------------------------
main_loop(TCPSocket, Owner, Receiver, MailBox) ->
	receive
		% WebSocket API
		?SEND_REQ(From, Data) ->
			Reply = send_frame(TCPSocket, Data),
			From ! Reply,
			main_loop(TCPSocket, Owner, Receiver, MailBox);
		?RECV_REQ(From, _Timeout) ->
			{Reply, NewMailBox} = recv_frame(MailBox),
			From ! Reply,
			main_loop(TCPSocket, Owner, Receiver, NewMailBox);
		?CHANGE_OWNER(Owner, NewOwner) ->
			Owner ! ?CHANGE_OWNER_OK,
			main_loop(TCPSocket, NewOwner, Receiver, MailBox);
		?CHANGE_OWNER(From, _) ->
			From ! ?CHANGE_OWNER_ERROR(not_owner),
			main_loop(TCPSocket, Owner, Receiver, MailBox);
		?CLOSE_REQ ->
			gen_tcp:close(TCPSocket),
			Owner ! ?WS_CLOSE_SIGNAL,
			main_end_loop(MailBox);
		% Receiver Update
		?RECV_NEW(Receiver, Msg) ->
			main_loop(TCPSocket, Owner, Receiver, queue:in(Msg, MailBox));
		?RECV_CLOSE(Receiver) ->
			gen_tcp:close(TCPSocket),
			Owner ! ?WS_CLOSE_SIGNAL,
			main_end_loop(MailBox);
		% Other
		X ->
			?print("main_loop", X),
			main_loop(TCPSocket, Owner, Receiver, MailBox)
	end.
%------------------------------------------------------------------------------
main_end_loop(MailBox) ->
	?print("terminate", MailBox).
%------------------------------------------------------------------------------
send_frame(TCPSocket, Data) ->
	?FRAME_SUCESS(Frame) = hixie_frame:frame({text, Data}),
	case gen_tcp:send(TCPSocket, Frame) of
		ok -> ?SEND_RES_OK;
		{error, Reason} -> ?SEND_RES_ERROR(Reason)
	end.
%------------------------------------------------------------------------------
recv_frame(MailBox) ->
	case queue:out(MailBox) of
		{{value, Message}, NewMailBox} ->
			{?RECV_RES(Message), NewMailBox};
		{empty, MailBox} ->
			{?RECV_RES(empty), MailBox}
	end.
%------------------------------------------------------------------------------
receiver_start(TCPSocket, Handler) ->
	receiver_loop(TCPSocket, Handler, nil, []).
%------------------------------------------------------------------------------
receiver_loop(TCPSocket, Handler, Context, []) ->
	case gen_tcp:recv(TCPSocket, ?ALL) of
		{ok, Stream} ->
			receiver_unframe(TCPSocket, Handler, Context, Stream);
		{error, closed} ->
			Handler ! ?RECV_CLOSE;
		X ->
			?print("rcv_loop", X),
			receiver_loop(TCPSocket, Handler, Context, [])
	end;
receiver_loop(TCPSocket, Handler, Context, Buffer) ->
	receiver_unframe(TCPSocket, Handler, Context, Buffer).
%------------------------------------------------------------------------------
receiver_unframe(TCPSocket, Handler, Context, Stream) ->
	case hixie_frame:unframe(Stream, Context) of
		% Close Signal
		?UNFRAME_SUCESS(?FRAME_SIGN(?SIGN_CLOSE), _) ->
			Handler ! ?RECV_CLOSE;
		% Only Text Frames
		?UNFRAME_SUCESS({text, Data}, Buffer) ->
			Handler ! ?RECV_NEW(Data),
			receiver_loop(TCPSocket, Handler, nil, Buffer);
		% Ignore All Other Frames
		?UNFRAME_SUCESS(_, Buffer) ->
			receiver_loop(TCPSocket, Handler, nil, Buffer);
		% Partial UnFrame
		?UNFRAME_PARTIAL(Context, Buffer) ->
			receiver_loop(TCPSocket, Handler, Context, Buffer);
		% Invalid Frame
		?UNFRAME_ERROR(_, _) ->
			Handler ! ?RECV_CLOSE;
		X ->
			?print("error", X)
	end.
%------------------------------------------------------------------------------
listen_start(Port, Owner) ->
	case gen_tcp:listen(Port, ?TCP_OPT) of
		{ok, TCPListen} ->
			Owner ! {self(), ok},
			listen_loop(TCPListen, Owner);
		{error, Reason} ->
			Owner ! {self(), Reason}
	end.
%------------------------------------------------------------------------------
listen_loop(TCPListen, Owner) ->
	receive
		?ACCEPT_REQ(From, Timeout) ->
			accept_socket(TCPListen, From, Timeout),
			listen_loop(TCPListen, Owner);
		?CHANGE_OWNER(Owner, NewOwner) ->
			Owner ! ?CHANGE_OWNER_OK,
			listen_loop(TCPListen, NewOwner);
		?CHANGE_OWNER(From, _) ->
			From ! ?CHANGE_OWNER_ERROR(not_owner),
			listen_loop(TCPListen, Owner);
		?CLOSE_REQ ->
			gen_tcp:close(TCPListen),
			Owner ! ?WS_CLOSE_SIGNAL,
			listen_end_loop(Owner);
		X ->
			?print("listen_loop", X),
			listen_loop(TCPListen, Owner)
	end.
%------------------------------------------------------------------------------			
listen_end_loop(_) ->
	ok.
%------------------------------------------------------------------------------
accept_socket(TCPListen, SocketOwner, Timeout) ->
	case gen_tcp:accept(TCPListen, Timeout) of
		{ok, TCPSocket} ->
			accept_request(TCPSocket, SocketOwner);
		{error, Reason} ->
			SocketOwner ! ?ACCEPT_RES_ERROR(Reason)
	end.
%------------------------------------------------------------------------------
accept_request(TCPSocket, SocketOwner) ->
	Reply = 
	case catch(accept_request_1(TCPSocket, SocketOwner)) of
		{error, {error, Reason}} ->
			?ACCEPT_RES_ERROR(Reason);
		{'EXIT', {{case_clause, Reason}, _}} ->
			?ACCEPT_RES_ERROR(Reason);
		{'EXIT', {Reason, _}} ->
			?ACCEPT_RES_ERROR(Reason);
		WebSocket ->
			?ACCEPT_RES_OK(WebSocket)
	end,
	SocketOwner ! Reply.
%------------------------------------------------------------------------------
accept_request_1(TCPSocket, SocketOwner) ->
	RequestHeader = receive_header(TCPSocket),
	Key3 = receive_key3(TCPSocket),

	Request = ws_header:parse(RequestHeader ++ Key3),
	Response = hixie76_lib:gen_response(Request),

	SubProtocol = hixie76_lib:get_subprotocol(Response),

	ResponseHeader = ws_header:to_string(Response),
	case gen_tcp:send(TCPSocket, ResponseHeader) of
		ok ->
			accept_socket_ok(
				TCPSocket, SocketOwner, 
				{Request, Response, SubProtocol});
		{error, Reason} ->
			erlang:error(Reason)
	end.
%------------------------------------------------------------------------------
receive_key3(TCPSocket) ->
	receive_len(TCPSocket, ?KEY3_LEN, []).
%------------------------------------------------------------------------------
accept_socket_ok(TCPSocket, SocketOwner, {Request, Response, SubProtocol}) ->
	HandlerParam = 
		[{request, Request},
		 {response, Response}, 
		 {subprotocol, SubProtocol}],
	create_websocket_handler(TCPSocket, HandlerParam, SocketOwner).
%------------------------------------------------------------------------------

