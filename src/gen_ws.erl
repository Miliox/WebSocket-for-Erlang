%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : WebSocket equivalente ao Socket Comum
%% Criado: 05/17/11 20:00:39 (HPC)
%% Copyright Emiliano@2011
%------------------------------------------------------------------------------
-module(gen_ws).
-author("elmiliox@gmail.com").
-vsn(5).
%------------------------------------------------------------------------------
-include("gen_ws.hrl").
-include("wslib/frame.hrl").
%------------------------------------------------------------------------------
-import(erlang).
% Data Structure
-import(lists).
-import(queue).
-import(socket).
% WebSocket Library
-import(wslib.url).
-import(wslib.header).
-import(wslib.hixie76).
%------------------------------------------------------------------------------
% WebSocket API
-export([recv/1, recv/2]).
-export([send/2, send/3]).
-export([connect/1, connect/2]).
% Listen WebSocket API
-export([accept/1, accept/2]).
-export([listen/1, listen/2]).
% Commom API
-export([close/1]).
-export([getinfo/1]).
-export([controlling_process/2]).
%------------------------------------------------------------------------------
%% Cria um WebSocket a ser usado pelo Cliente
connect(Url) ->
	connect(Url, ?DEF_CON_OPT).
connect(Url, Opt) when is_list(Url) andalso is_list(Opt) ->
	{Active, Origin, SubProtocol, Timeout} = get_connect_opt(Opt),
	{Mode, Address, _, Port, _} = url:parse(Url),
	
	case catch(socket:connect(Mode, Address, Port, Timeout)) of
		{ok, Socket} ->
			%make_handshake(Url, Origin, SubProtocol, Active, Socket);
			handler.hixie76:new(Url, Origin, SubProtocol, Active, Socket);
		?ERROR(Reason, _) ->
			{error, Reason};
		?ERROR(Reason) ->
			{error, Reason};
		Error ->
			Error
	end;
connect(_,_) ->
	{error, badarg}.
%------------------------------------------------------------------------------
%% Cria um WebSocket a ser usado pelo Servidor
listen(Port) ->
	listen(Port, ?DEF_LST_OPT).
listen(Port, Options) when is_list(Options) ->
	Mode = get_opt(mode, Options),
	ListenPid = spawn(?MODULE, listen_start, [Port, Mode, self()]),
	receive
		?LISTEN_OK(ListenPid) ->
			{ok, ?WSL_FMT(ListenPid)};
		?LISTEN_ERROR(ListenPid, Reason) ->
			{error, Reason}
	end;
listen(_, _) ->
	{error, badarg}.
%------------------------------------------------------------------------------
%% Efetua o HandShake e estabelece uma Conexao WebSocket
accept(ListenWebSocket) ->
	accept(ListenWebSocket, infinity).
accept(?WSL_FMT(ListenPid), Timeout) ->
	ListenPid ! ?ACCEPT_REQ(Timeout),
	receive
		?ACCEPT_RES_OK(ListenPid, WebSocket) ->
			{ok, WebSocket};
		?ACCEPT_RES_ERROR(ListenPid, closed) ->
			{error, badsock};
		?ACCEPT_RES_ERROR(ListenPid, Reason) ->
			{error, Reason}
	end;
accept(_, _) ->
	{error, badarg}.
%------------------------------------------------------------------------------
%% Recebe uma mensagem transmitida via WebSocket
recv(WebSocket) ->
	recv(WebSocket, infinity).
%------------------------------------------------------------------------------
recv(?WS_FMT(Handler), Timeout) 
when Timeout == infinity orelse Timeout > 0 ->
	Handler ! ?RECV_REQ(Timeout),
	receive
		?RECV_RES_OK(Handler, Data) -> 
			{ok, Data};
		?RECV_RES_ERROR(Handler, Reason) ->
			{error, Reason}
	after Timeout ->
		{error, timeout}
	end;
recv(_, _) ->
	{error, badarg}.
%------------------------------------------------------------------------------
%% Envia uma mensagem via WebSocket
send(WebSocket, Data) ->
	send(WebSocket, Data, infinity).
%------------------------------------------------------------------------------
send(?WS_FMT(Handler), Data, Timeout) 
when is_list(Data) andalso (Timeout == infinity orelse Timeout > 0) ->
	Handler ! ?SEND_REQ(Data),
	receive
		?SEND_RES_OK(Handler) -> 
			ok;
		?SEND_RES_ERROR(Handler, Reason) -> 
			{error, Reason}
	after Timeout ->
		{error, timeout}
	end;
send(_, Data, _) when is_binary(Data) ->
	{error, einval};
send(_, _, _) ->
	{error, badarg}.
%------------------------------------------------------------------------------
%% Encerra uma conexao WebSocket
close(?WS_FMT(Handler)) ->
	close_1(Handler);
close(?WSL_FMT(Handler)) ->
	close_1(Handler);
close(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
close_1(Handler) ->
	Handler ! ?CLOSE_REQ,
	ok.
%------------------------------------------------------------------------------
controlling_process(?WS_FMT(Handler), NewOwner) when is_pid(NewOwner) -> 
	Handler ! ?CHANGE_OWNER(NewOwner),
	receive
		?CHANGE_OWNER_OK(Handler) ->
			ok;
		?CHANGE_OWNER_ERROR(Handler, Reason) ->
			{error, Reason}
	end;
controlling_process(_, _) ->
	{error, badarg}.
%------------------------------------------------------------------------------
getinfo(?WS_FMT(Handler)) ->
	getinfo_1(Handler);
getinfo(?WSL_FMT(Handler)) ->
	getinfo_1(Handler).
%------------------------------------------------------------------------------
getinfo_1(Handler) ->
	Handler ! ?INFO_REQ,
	receive
		?INFO_RES(Handler, Info) -> lists:sort(Info)
	end.
%------------------------------------------------------------------------------
% Internal Functions
%------------------------------------------------------------------------------
get_connect_opt(Options) ->
	Active  = get_opt(active, Options),
	Origin  = get_opt(origin, Options),
	SubProtocol = get_opt(subprotocol, Options),
	Timeout = get_opt(timeout, Options),

	{Active, Origin, SubProtocol, Timeout}.
%------------------------------------------------------------------------------
get_opt(Key, []) ->
	default_opt(Key);
get_opt(Key, [{Key, Value}|_]) ->
	Value;
get_opt(Key, [_|Dict]) ->
	get_opt(Key, Dict).
%------------------------------------------------------------------------------
default_opt(active) -> 
	?DEF_ACTIVE;
default_opt(mode) ->
	?DEF_MODE;
default_opt(origin) -> 
	?DEF_ORIGIN;
default_opt(subprotocol) -> 
	?DEF_SUBP;
default_opt(timeout) -> 
	?DEF_TIMEOUT;
default_opt(_) -> 
	erlang:error(badarg).
%------------------------------------------------------------------------------
make_handshake(Url, Origin, SubProtocol, Active, Socket) ->
	{Request, Answer} = 
		hixie76:gen_request(Url, Origin, SubProtocol),
	RequestHeader = 
		header:to_string(Request),

	HandlerParameter = 
		[{active, Active}, {origin, Origin}, {request, Request}, {url, Url}],
		
	send_handshake(Socket, RequestHeader, Answer, HandlerParameter).
%------------------------------------------------------------------------------
send_handshake(Socket, RequestHeader, Answer, HandlerParameter) ->
	case socket:send(Socket, RequestHeader) of
		ok ->
			receive_response(Socket, Answer, HandlerParameter);
		Error ->
			Error
	end.
%------------------------------------------------------------------------------
receive_response(Socket, Answer, HandlerParameter) ->
	case catch(receive_response_1(Socket, Answer, HandlerParameter)) of
		?ERROR(Reason, _) ->
			receive_response_error(Socket, Reason);
		?ERROR(Reason) ->
			receive_response_error(Socket, Reason);
		Sucess ->
			Sucess
	end.
%------------------------------------------------------------------------------
receive_response_error(Socket, Reason) ->
	close_graceful(Socket),
	{error, Reason}.
%------------------------------------------------------------------------------
receive_response_1(Socket, Answer, HandlerParameter) ->
	ResponseHeader = receive_header(Socket),
	ServerSolution = receive_response_solution(Socket),

	Response = 
		header:parse(ResponseHeader ++ ServerSolution),
	SubProtocol = 
		hixie76:get_subprotocol(Response),
	
	verify_response(ServerSolution, Answer, Socket, HandlerParameter ++ 
			[{response, Response}, {subprotocol, SubProtocol}]).
%------------------------------------------------------------------------------
verify_response(ServerSolution, Answer, Socket, HandlerParameter) ->
	case ServerSolution == Answer of
		true ->
			{ok, create_websocket(Socket, HandlerParameter)};
		false ->
			close_graceful(Socket),
			{error, response}
	end.	
%------------------------------------------------------------------------------
receive_header(Socket) ->
	receive_header_loop(Socket, [], 0).
%------------------------------------------------------------------------------
% Header Loop Termina quanto encontrar CRLFCRLF
receive_header_loop(Socket, RevBuffer, Len) 
when Len < ?MAX_HEADER_LEN ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?CR}  -> 
			receive_header_loop_1(Socket, ?CR++RevBuffer, Len+1);
		{ok, Char} -> 
			receive_header_loop(Socket, Char++RevBuffer, Len+1);
		{error, Reason} -> 
			erlang:error(Reason)
	end;
receive_header_loop(_, _, _) ->
	erlang:error(header).
%------------------------------------------------------------------------------
% Falta LFCRLF
receive_header_loop_1(Socket, RevBuffer, Len) 
when Len < ?MAX_HEADER_LEN ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?LF}  -> 
			receive_header_loop_2(Socket, ?LF++RevBuffer, Len+1);
		{ok, Char} -> 
			receive_header_loop(Socket, Char++RevBuffer, Len+1);
		{error, Reason} -> 
			erlang:error(Reason)
	end;
receive_header_loop_1(_, _, _) ->
	erlang:error(header).
%------------------------------------------------------------------------------
% Falta CRLF
receive_header_loop_2(Socket, RevBuffer, Len) 
when Len < ?MAX_HEADER_LEN ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?CR}  -> 
			receive_header_loop_3(Socket, ?CR++RevBuffer, Len+1);
		{ok, Char} -> 
			receive_header_loop(Socket, Char++RevBuffer, Len+1);
		{error, Reason} -> 
			erlang:error(Reason)
	end;
receive_header_loop_2(_, _, _) ->
	erlang:error(header).
%------------------------------------------------------------------------------
% Falta LF
receive_header_loop_3(Socket, RevBuffer, Len) 
when Len < ?MAX_HEADER_LEN ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?LF}  -> 
			lists:reverse(?LF++RevBuffer);
		{ok, Char} -> 
			receive_header_loop(Socket, Char++RevBuffer, Len+1);
		{error, Reason} -> 
			erlang:error(Reason)
	end;
receive_header_loop_3(_, _, _) ->
	erlang:error(header).
%------------------------------------------------------------------------------
receive_response_solution(Socket) ->
	receive_len(Socket, ?SOLUTION_LEN, []).
%------------------------------------------------------------------------------
receive_len(_, Len, RevBuffer) when Len =< 0 ->
	lists:reverse(RevBuffer);
receive_len(Socket, Len, RevBuffer) ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, Byte} -> 
			receive_len(Socket, Len-1, Byte++RevBuffer);
		{error, Reason} -> 
			erlang:error(Reason)
	end.
%------------------------------------------------------------------------------
create_websocket(Socket, HandlerParameter) ->
	create_websocket(Socket, HandlerParameter, self()).
create_websocket(Socket, HandlerParameter, Owner) ->
	HandlerPid = spawn(?MODULE, handler_start, [Socket, Owner, HandlerParameter]),
	socket:controlling_process(Socket, HandlerPid),

	?WS_FMT(HandlerPid).
%------------------------------------------------------------------------------
handler_start(Socket, Owner, HandlerParameter) ->
	handler_load(HandlerParameter),
	handler_load_info_sock(Socket),

	MailBox = queue:new(),
	Receiver = spawn_link(?MODULE, receiver_start, [Socket, self()]),

	handler_loop(Socket, Owner, Receiver, MailBox).
%------------------------------------------------------------------------------
handler_load([]) -> 
	ok;
handler_load([{Key, Value}|Parameter]) -> 
	put(Key, Value),
	handler_load(Parameter);
handler_load([_|Parameter]) ->
	handler_load(Parameter).
%------------------------------------------------------------------------------
handler_load_info_sock(Socket) ->
	{HostAddr, HostPort} = socket:sockname(Socket),
	{PeerAddr, PeerPort} = socket:peername(Socket),

	put(host_addr, HostAddr),
	put(peer_addr, PeerAddr),
	put(host_port, HostPort),
	put(peer_port, PeerPort).
%------------------------------------------------------------------------------
handler_loop(Socket, Owner, Receiver, MailBox) ->
receive
	% WebSocket API
	?SEND_REQ(From, Data) ->
		Reply = send_frame(Socket, Data),
		From ! Reply,
		handler_loop(Socket, Owner, Receiver, MailBox);
	?RECV_REQ(From, _Timeout) ->
		{Reply, NewMailBox} = recv_frame(MailBox),
		From ! Reply,
		handler_loop(Socket, Owner, Receiver, NewMailBox);
	?CHANGE_OWNER(Owner, NewOwner) ->
		Owner ! ?CHANGE_OWNER_OK,
		handler_loop(Socket, NewOwner, Receiver, MailBox);
	?CHANGE_OWNER(From, _) ->
		From ! ?CHANGE_OWNER_ERROR(not_owner),
		handler_loop(Socket, Owner, Receiver, MailBox);
	?CLOSE_REQ ->
		close_graceful(Socket),
		Owner ! ?WS_CLOSE_SIGNAL,
		handler_end_loop(MailBox);
	?INFO_REQ(From) ->
		From ! ?INFO_RES(get()),
		handler_loop(Socket, Owner, Receiver, MailBox);
	% Receiver Update
	?RECV_NEW(Receiver, Msg) ->
		NewMailBox = handler_loop_recv(Msg, Owner, MailBox),
		handler_loop(Socket, Owner, Receiver, NewMailBox);
	?RECV_CLOSE(Receiver) ->
		socket:close(Socket),
		Owner ! ?WS_CLOSE_SIGNAL,
		handler_end_loop(MailBox);
	% Other
	X ->
		?print("handler_loop", X),
		handler_loop(Socket, Owner, Receiver, MailBox)
end.
%------------------------------------------------------------------------------
handler_loop_recv(Msg, Owner, MailBox) ->
	case get(active) of
		false ->
			queue:in(Msg, MailBox);
		_True ->
			Owner ! ?WS_RECV_SIGNAL(Msg), 
			MailBox
	end.
%------------------------------------------------------------------------------
handler_end_loop(MailBox) ->
receive
	?ACCEPT_REQ(From, _) ->
		From ! ?ACCEPT_RES_ERROR(closed),
		handler_end_loop(MailBox);
	?RECV_REQ(From, _) ->
		NewMailBox = end_recv_frame(From, MailBox),
		handler_end_loop(NewMailBox);
	?SEND_REQ(From, _) ->
		From !?SEND_RES_ERROR(closed),
		handler_end_loop(MailBox);
	?CHANGE_OWNER(From, _) ->
		From ! {error, closed},
		handler_end_loop(MailBox);
	?INFO_REQ(From) ->
		From ! ?INFO_RES(get()),
		handler_end_loop(MailBox);
	_ ->
		handler_end_loop(MailBox)
end.
%------------------------------------------------------------------------------
end_recv_frame(From, MailBox) ->
	case queue:out(MailBox) of
		{empty, NewMailBox} ->
			From ! ?RECV_RES_ERROR(closed);
		{{value, Msg}, NewMailBox} ->
			From ! ?RECV_RES_OK(Msg)
	end,
	NewMailBox.
%------------------------------------------------------------------------------
send_frame(Socket, Data) ->
	?FRAME_SUCESS(Frame) = hixie76:frame({text, Data}),
	case socket:send(Socket, Frame) of
		ok -> 
			?SEND_RES_OK;
		{error, Reason} -> 
			?SEND_RES_ERROR(Reason)
	end.
%------------------------------------------------------------------------------
recv_frame(MailBox) ->
	case queue:out(MailBox) of
		{{value, Message}, NewMailBox} ->
			{?RECV_RES_OK(Message), NewMailBox};
		{empty, MailBox} ->
			{?RECV_RES_OK(empty), MailBox}
	end.
%------------------------------------------------------------------------------
receiver_start(Socket, Handler) ->
	receiver_loop(Socket, Handler, nil, []).
%------------------------------------------------------------------------------
receiver_loop(Socket, Handler, Context, []) ->
	case socket:recv(Socket, ?ALL) of
		{ok, Stream} ->
			receiver_unframe(Socket, Handler, Context, Stream);
		{error, closed} ->
			Handler ! ?RECV_CLOSE;
		X ->
			?print("rcv_loop", X),
			receiver_loop(Socket, Handler, Context, [])
	end;
receiver_loop(Socket, Handler, Context, Buffer) ->
	receiver_unframe(Socket, Handler, Context, Buffer).
%------------------------------------------------------------------------------
receiver_unframe(Socket, Handler, Context, Stream) ->
	case hixie76:unframe(Stream, Context) of
		% Close Signal
		?UNFRAME_SUCESS(?FRAME_SIGN(?SIGN_CLOSE), _) ->
			Handler ! ?RECV_CLOSE;
		% Only Text Frames
		?UNFRAME_SUCESS({text, Data}, Buffer) ->
			Handler ! ?RECV_NEW(Data),
			receiver_loop(Socket, Handler, nil, Buffer);
		% Ignore All Other Frames
		?UNFRAME_SUCESS(_, Buffer) ->
			receiver_loop(Socket, Handler, nil, Buffer);
		% Partial UnFrame
		?UNFRAME_PARTIAL(Context, Buffer) ->
			receiver_loop(Socket, Handler, Context, Buffer);
		% Invalid Frame
		?UNFRAME_ERROR(_, _) ->
			Handler ! ?RECV_CLOSE;
		X ->
			?print("error", X)
	end.
%------------------------------------------------------------------------------
listen_start(Port, Mode, Owner) ->
	case catch(socket:listen(Mode, Port)) of
		{ok, Listen} ->
			Owner ! ?LISTEN_OK,
			listen_loop(Listen, Owner);
		{error, Reason} ->
			Owner ! ?LISTEN_ERROR(Reason);
		?ERROR(Reason, _) ->
			Owner ! ?LISTEN_ERROR(Reason);
		?ERROR(Reason) ->
			Owner ! ?LISTEN_ERROR(Reason)
	end.
%------------------------------------------------------------------------------
listen_loop(Listen, Owner) ->
receive
	?ACCEPT_REQ(From, Timeout) ->
		accept_socket(Listen, From, Timeout),
		listen_loop(Listen, Owner);
	?CHANGE_OWNER(Owner, NewOwner) ->
		Owner ! ?CHANGE_OWNER_OK,
		listen_loop(Listen, NewOwner);
	?CHANGE_OWNER(From, _) ->
		From ! ?CHANGE_OWNER_ERROR(not_owner),
		listen_loop(Listen, Owner);
	?INFO_REQ(From) ->
		From ! ?INFO_RES(get()),
		listen_loop(Listen, Owner);
	?CLOSE_REQ ->
		socket:close(Listen),
		Owner ! ?WS_CLOSE_SIGNAL,
		listen_end_loop(Owner);
	X ->
		?print("listen_loop", X),
		listen_loop(Listen, Owner)
end.
%------------------------------------------------------------------------------			
listen_end_loop(_) ->
	handler_end_loop(queue:new()).
%------------------------------------------------------------------------------
accept_socket(Listen, SocketOwner, Timeout) ->
	case socket:accept(Listen, Timeout) of
		{ok, Socket} ->
			accept_request(Socket, SocketOwner);
		{error, Reason} ->
			SocketOwner ! ?ACCEPT_RES_ERROR(Reason)
	end.
%------------------------------------------------------------------------------
accept_request(Socket, SocketOwner) ->
	Reply = 
	case catch(accept_request_1(Socket, SocketOwner)) of
		?ERROR(Reason, _) ->
			accept_request_error(Socket, Reason);
		?ERROR(Reason) ->
			accept_request_error(Socket, Reason);
		WebSocket ->
			?ACCEPT_RES_OK(WebSocket)
	end,
	SocketOwner ! Reply.
%------------------------------------------------------------------------------
accept_request_error(Socket, Reason) ->
	socket:send(Socket, ?RESPONSE_ERROR),
	socket:close(Socket),

	?ACCEPT_RES_ERROR(Reason).
%------------------------------------------------------------------------------
accept_request_1(Socket, SocketOwner) ->
	RequestHeader = receive_header(Socket),
	Key3 = receive_key3(Socket),

	Request = header:parse(RequestHeader ++ Key3),
	Response = hixie76:gen_response(Request, socket:mode(Socket)),

	SubProtocol = hixie76:get_subprotocol(Response),

	ResponseHeader = header:to_string(Response),
	case socket:send(Socket, ResponseHeader) of
		ok ->
			accept_socket_ok(
				Socket, SocketOwner, 
				{Request, Response, SubProtocol});
		{error, Reason} ->
			erlang:error(Reason)
	end.
%------------------------------------------------------------------------------
receive_key3(Socket) ->
	receive_len(Socket, ?KEY3_LEN, []).
%------------------------------------------------------------------------------
accept_socket_ok(Socket, SocketOwner, {Request, Response, SubProtocol}) ->
	HandlerParam = [
		{active, false},
		{request, Request},
		{response, Response},
		{subprotocol, SubProtocol}],
	
	create_websocket(Socket, HandlerParam, SocketOwner).
%------------------------------------------------------------------------------
close_graceful(Socket) ->
	socket:send(Socket, ?CLOSE_FRAME),
	socket:close(Socket).
%------------------------------------------------------------------------------
