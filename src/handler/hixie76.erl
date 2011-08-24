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
-import(io).
-import(socket).
-import(queue).
-import(lists).
-import(wslib.header).
-import(wslib.hixie76).
-import(handler.common).
%------------------------------------------------------------------------------
-export([new/5]).
%------------------------------------------------------------------------------
new(Url, Origin, SubProtocol, Active, Socket) ->
	Config = {Url, Origin, SubProtocol, Active},
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
	set_dict(Config),
	common:load_info_sock(Socket),
	handler_wait_handshake(Socket, Owner, Receiver).
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

			io:format("~p", [get()]),
			handler_main(Socket, Owner, Receiver);
		{Receiver, error, _Reason} ->
			Owner ! ?WS_CLOSE_SIGNAL,
			socket:close(Socket),
			handler_dead();
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
		_Other ->
			handler_wait_handshake(Socket, Owner, Receiver)
	end.
%------------------------------------------------------------------------------
handler_main(Socket, Owner, Receiver) ->
	handler_main(Socket, Owner, Receiver, queue:new()).
%------------------------------------------------------------------------------
handler_main(Socket, Owner, Receiver, Messages) ->
	receive
		% Client
		?SEND_REQ(From, Data) ->
			From ! send_frame(Socket, Data),
			handler_main(Socket, Owner, Receiver, Messages);
		?RECV_REQ(From, _Timeout) ->
			%TODO:
			From ! ?RECV_RES_OK(empty),
			handler_main(Socket, Owner, Receiver, Messages);
		?CHANGE_OWNER(Owner, NewOwner) when is_pid(NewOwner) ->
			Owner ! ?CHANGE_OWNER_OK,
			handler_main(Socket, NewOwner, Receiver, Messages);
		?CHANGE_OWNER(From, _) ->
			From ! ?CHANGE_OWNER_ERROR(not_owner),
			handler_main(Socket, Owner, Receiver, Messages);
		?CLOSE_REQ ->
			close_graceful(Socket),
			Owner ! ?WS_CLOSE_SIGNAL,
			handler_dead(Messages);
		?INFO_REQ(From) ->
			From ! ?INFO_RES(get()),
			handler_main(Socket, Owner, Receiver, Messages);
		% Receiver
		?RECV_NEW(Receiver, Msg) ->
			%TODO: Active & Inactive
			Owner ! ?RECV_RES_OK(Msg),
			handler_main(Socket, Owner, Receiver, Messages);
		?RECV_CLOSE(Receiver) ->
			socket:close(Socket),
			Owner ! ?WS_CLOSE_SIGNAL,
			handler_dead(Messages);
		_Other ->
			handler_main(Socket, Owner, Receiver, Messages)
	end.
%------------------------------------------------------------------------------
handler_dead() ->
		handler_dead(queue:new()).
%------------------------------------------------------------------------------
handler_dead(Messages) ->
	receive
		?SEND_REQ(From, _) ->
			From ! ?ACCEPT_RES_ERROR(closed),
			handler_dead(Messages);
		?RECV_REQ(From, _) ->
			% TODO
			From ! ?RECV_RES_ERROR(closed),
			handler_dead(Messages);
		?ACCEPT_REQ(From, _) ->
			From ! ?ACCEPT_RES_ERROR(closed),
			handler_dead(Messages);
		?CHANGE_OWNER(From, _) ->
			From ! ?CHANGE_OWNER_ERROR(closed),
			handler_dead(Messages);
		?INFO_REQ(From) ->
			From ! ?INFO_RES(get()),
			handler_dead(Messages);
		_Other ->
			% TODO
			handler_dead(Messages)
	end.
%------------------------------------------------------------------------------
put_key(Key, TupleList) ->
	case lists:keyfind(Key, 1, TupleList) of
		{Key, Value} ->	put(Key, Value);
		_ -> true
	end.
%------------------------------------------------------------------------------
receiver_start(Socket, Config, Handler) ->
	{U, O, S, _} = Config,
	case make_handshake(U, O, S, Socket) of
		{error, Reason} ->
			Handler ! {self(), error, Reason};
		{ok, Handshake} ->
			Handler ! {self(), ok, Handshake},
			receiver_loop(Socket, Handler)
	end.
%------------------------------------------------------------------------------
make_handshake(Url, Origin, SubProtocol, Socket) ->
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
	ResponseHeader = common:receive_header(Socket),
	ServerSolution = receive_response_solution(Socket),

	Response = 
		header:parse(ResponseHeader ++ ServerSolution),
	SubProtocol = 
		hixie76:get_subprotocol(Response),
	
	verify_response(
			ServerSolution, Answer, Socket, 
			HandshakeParameter ++ 
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
receive_response_solution(Socket) ->
	common:receive_len(Socket, ?SOLUTION_LEN, []).
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
send_frame(Socket, Data) ->
	?FRAME_SUCESS(Frame) = wslib.hixie76:frame({text, Data}),
	case socket:send(Socket, Frame) of
		ok -> 
			?SEND_RES_OK;
		{error, Reason} -> 
			?SEND_RES_ERROR(Reason)
	end.
%------------------------------------------------------------------------------
close_graceful(Socket) ->
	socket:send(Socket, ?CLOSE_FRAME),
	socket:close(Socket).
%------------------------------------------------------------------------------
receiver_loop(Socket, Handler) ->
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
	case wslib.hixie76:unframe(Stream, Context) of
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
