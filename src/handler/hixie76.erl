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
	load_info_sock(Socket),
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
			% handler_main_start(Socket, Owner, Receiver);
			close_graceful(Socket);
		{Receiver, error, Reason} ->
			Owner ! ?WS_CLOSE_SIGNAL,
			socket:close(Socket),

			io:format("fail ~p", [Reason]);
			%handler_end_loop(queue:new());
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
	_ ->
			handler_wait_handshake(Socket, Owner, Receiver)
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
			Handler ! {self(), ok, Handshake}
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
load_info_sock(Socket) ->
	{HostAddr, HostPort} = socket:sockname(Socket),
	{PeerAddr, PeerPort} = socket:peername(Socket),

	put(host_addr, HostAddr),
	put(peer_addr, PeerAddr),
	put(host_port, HostPort),
	put(peer_port, PeerPort).
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
close_graceful(Socket) ->
	socket:send(Socket, ?CLOSE_FRAME),
	socket:close(Socket).
%------------------------------------------------------------------------------
