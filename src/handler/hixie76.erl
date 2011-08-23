%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 08/22/11 12:38:59 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011
%------------------------------------------------------------------------------
-module(handler.hixie76).
-author("elmiliox@gmail.com").
-vsn(1).
%------------------------------------------------------------------------------
-include("gen_ws.hrl").
-include("wslib/frame.hrl").
%------------------------------------------------------------------------------
-import(socket).
-import(queue).
-import(lists).
-import(wslib.header).
-import(wslib.hixie76).
%------------------------------------------------------------------------------
new(Url, Origin, SubProtocol, Active, Socket) ->
	Config = {Url, Origin, SubProtocol, Active},
	set_dict(Config),
	HandlerPid = spawn_handler(Socket, Config),
	?WS_FMT(HandlerPid).
%------------------------------------------------------------------------------
set_dict({Url, Origin, SubProtocol, Active}) ->
	put(url, Url),
	put(active, Active),
	put(origin, Origin),
	put(subprotocol, SubProtocol).
%------------------------------------------------------------------------------
spawn_handler(Socket, Config) ->
	Handler = spawn(?MODULE, handler_start, [Socket, Config, self()]),
	socket:controlling_process(Socket, Handler),
	Handler.
%------------------------------------------------------------------------------
handler_start(Socket, Config, Owner) ->
	Receiver = spawn_receiver(Socket, Config),

	handler_wait_handshake(Receiver, Owner).
%------------------------------------------------------------------------------
spawn_receiver(Socket, Config) ->
	Receiver = spawn_link(?MODULE, receiver_start, [Socket, Config, self()]),
	socket:controlling_process(Socket, Receiver), 
	
	Receiver.
%------------------------------------------------------------------------------
handler_wait_handshake(Socket, Owner, Receiver) ->
	receive
		{Receiver, ok, Handshake} ->
			Owner ! ?WS_CONNECT_SIGNAL,

			put_key(request, Handshake),
			put_key(response, Handshake),
			put_key(subprotocol, Handshake),

			handler_main_start(Socket, Owner, Receiver);
		{Receiver, error, _Reason} ->
			Owner ! ?WS_CLOSE_SIGNAL,
			socket:close(Socket),
			handler_end_loop(queue:new());
		% Client Request
		?ACCEPT_REQ(From, _) ->
			From ! ?ACCEPT_RES_ERROR(istate),
			handler_wait_handshake(Socket, Owner, Receiver);
		?SEND_REQ(From, _) ->
			From ! ?SEND_RES_ERROR(istate),
			handler_wait_handshake(Socket, Owner, Receiver);
		?CHANGE_OWNER(Owner, NewOwner) when is_pid(NewOwner)->
			Owner ! ?CHANGE_OWNER_OK,
			handler_wait_handshake(Socket, NewOwner, Receiver);
		?CHANGE_OWNER(From, _) ->
			From ! ?CHANGE_OWNER_ERROR(not_owner),
			handler_wait_handshake(Socket, Owner, Receiver);
		?INFO_REQ(From) ->
			From ! ?INFO_RES(get()),
			handler_wait_handshake(Socket, Owner, Receiver);
		
	end.
%------------------------------------------------------------------------------
put_key(Key, TupleList) ->
	case lists:keyfind() of
		{Key, Value} ->	put(Key, Value);
		_ -> true
	end.
%------------------------------------------------------------------------------
receiver_start(Socket, Config, Handler) ->
	{U, O, S, A} = Config,
	case make_handshake(U, O, S, A, Socket) of
		{error, Reason} ->
			Handler ! {self(), error, Reason};
		{ok, Handshake} ->
			Handler ! {self(), ok, Handshake}
	end.
%------------------------------------------------------------------------------
make_handshake(Url, Origin, SubProtocol, Active, Socket) ->
	{Request, Answer} = 
		hixie76:gen_request(Url, Origin, SubProtocol),
	RequestHeader = 
		header:to_string(Request),

	HandshakeParameter = [{request, Request}],
		
	send_handshake(Socket, RequestHeader, Answer, HandshakeParameter).
%------------------------------------------------------------------------------
send_handshake(Socket, RequestHeader, Answer, HandshakeParameter) ->
	case socket:send(Socket, RequestHeader) of
		ok ->
			receive_response(Socket, Answer, HandshakeParameter);
		Error ->
			Error
	end.
%------------------------------------------------------------------------------
receive_response(Socket, Answer, HandshakeParameter) ->
	case catch(receive_response_1(Socket, Answer, HandshakeParameter)) of
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
receive_response_1(Socket, Answer, HandshakeParameter) ->
	ResponseHeader = receive_header(Socket),
	ServerSolution = receive_response_solution(Socket),

	Response = 
		header:parse(ResponseHeader ++ ServerSolution),
	SubProtocol = 
		hixie76:get_subprotocol(Response),
	
	verify_response(ServerSolution, Answer, Socket, HandshakeParameter ++ 
			[{response, Response}, {subprotocol, SubProtocol}]).
%------------------------------------------------------------------------------
verify_response(ServerSolution, Answer, Socket, HandshakeParameter) ->
	case ServerSolution == Answer of
		true ->
			{ok, HandshakeParameter};
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
