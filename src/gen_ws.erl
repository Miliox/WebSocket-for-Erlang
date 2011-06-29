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
%-import(gen_tcp).
-import(hixie76_lib).
-import(socket).
%------------------------------------------------------------------------------
-export([connect/1, connect/2, listen/2]).
-export([accept/1, accept/2, recv/1, recv/2, send/2, send/3, close/1]).
-export([geturl/1, getsubprotocol/1, getstate/1]).
%------------------------------------------------------------------------------
%% Cria um WebSocket a ser usado pelo Cliente
connect(Url) ->
	connect(Url, ?DEF_CON_OPT).
connect(Url, Options) ->
	Origin = get_opt(origin, Options),
	Timeout = get_opt(timeout, Options),
	SubProtocol = get_opt(subprotocol, Options),
	{Mode, Address, _, Port, _} = ws_url:parse(Url),

	case socket:connect(Mode, Address, Port, Timeout) of
		{ok, Socket} ->
			make_handshake(Url, Origin, SubProtocol, Socket);
		Error ->
			Error
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
	listen(Port, normal).
listen(Port, Mode) ->
	listen(Port, Mode, []).
listen(Port, Mode,_Options) ->
	ListenPid = spawn(?MODULE, listen_start, [Port, Mode, self()]),
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
		?RECV_RES_OK(Handler, Data) -> 
			{ok, Data};
		?RECV_RES_ERROR(Handler, Reason) ->
			{error, Reason}
	after Timeout ->
		{error, timeout}
	end.
%------------------------------------------------------------------------------
%% Envia uma mensagem via WebSocket
send(WebSocket, Data) ->
	send(WebSocket, Data, infinity).
%------------------------------------------------------------------------------
send(?WS_FMT(Handler), Data, Timeout) when is_list(Data) ->
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
make_handshake(Url, Origin, SubProtocol, Socket) ->
	{Request, Answer} = hixie76_lib:gen_request(Url, Origin, SubProtocol),
	RequestHeader = ws_header:to_string(Request),

	HandlerParameter = [{url, Url}, {origin, Origin}, {request, Request}],

	case socket:send(Socket, RequestHeader) of
		ok ->
			receive_response(Socket, Answer, HandlerParameter);
		Error ->
			Error
	end.
%------------------------------------------------------------------------------
receive_response(Socket, Answer, HandlerParameter) ->
	case catch(receive_response_1(Socket, Answer, HandlerParameter)) of
		{'EXIT', {{case_clause, Error}, _}} ->
			socket:close(Socket),
			{error, Error};
		{'EXIT', {Reason, _}} ->
			socket:close(Socket),
			{error, Reason};
		Sucess ->
			Sucess
	end.
%------------------------------------------------------------------------------
receive_response_1(Socket, Answer, HandlerParameter) ->
	ResponseHeader = receive_header(Socket),
	ServerSolution = receive_response_solution(Socket),

	Response = ws_header:parse(ResponseHeader ++ ServerSolution),
	SubProtocol = hixie76_lib:get_subprotocol(Response),

	case ServerSolution == Answer of
		true ->
			WebSocket = create_websocket_handler(
				Socket, 
				HandlerParameter ++ 
					[{response, Response}, 
					 {subprotocol, SubProtocol}]),
			 {ok, WebSocket};
		false ->
			?REPLY_ERROR
	end.	
%------------------------------------------------------------------------------
receive_header(Socket) ->
	receive_header_loop(Socket, [], 0).
%------------------------------------------------------------------------------
% Header Loop Termina quanto encontrar CRLFCRLF
receive_header_loop(Socket, RevBuffer, Len) 
when Len < ?MAX_HEADER_LEN ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?CR}  -> receive_header_loop_1(Socket, ?CR++RevBuffer, Len+1);
		{ok, Char} -> receive_header_loop(Socket, Char++RevBuffer, Len+1);
		_ -> erlang:error(timeout)
	end;
receive_header_loop(_, _, _) ->
	erlang:error(header_overflow).
%------------------------------------------------------------------------------
% Falta LFCRLF
receive_header_loop_1(Socket, RevBuffer, Len) 
when Len < ?MAX_HEADER_LEN ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?LF}  -> receive_header_loop_2(Socket, ?LF++RevBuffer, Len+1);
		{ok, Char} -> receive_header_loop(Socket, Char++RevBuffer, Len+1);
		_ -> erlang:error(timeout)
	end;
receive_header_loop_1(_, _, _) ->
	erlang:error(header_overflow).
%------------------------------------------------------------------------------
% Falta CRLF
receive_header_loop_2(Socket, RevBuffer, Len) 
when Len < ?MAX_HEADER_LEN ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?CR}  -> receive_header_loop_3(Socket, ?CR++RevBuffer, Len+1);
		{ok, Char} -> receive_header_loop(Socket, Char++RevBuffer, Len+1);
		_ -> erlang:error(timeout)
	end;
receive_header_loop_2(_, _, _) ->
	erlang:error(header_overflow).
%------------------------------------------------------------------------------
% Falta LF
receive_header_loop_3(Socket, RevBuffer, Len) 
when Len < ?MAX_HEADER_LEN ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, ?LF}  -> lists:reverse(?LF++RevBuffer);
		{ok, Char} -> receive_header_loop(Socket, Char++RevBuffer, Len+1);
		_ -> erlang:error(timeout)
	end;
receive_header_loop_3(_, _, _) ->
	erlang:error(header_overflow).
%------------------------------------------------------------------------------
receive_response_solution(Socket) ->
	receive_len(Socket, ?SOLUTION_LEN, []).
%------------------------------------------------------------------------------
receive_len(_, Len, RevBuffer) when Len =< 0 ->
	lists:reverse(RevBuffer);
receive_len(Socket, Len, RevBuffer) ->
	case socket:recv(Socket, ?ONLY_ONE, ?HEADER_TIMEOUT) of
		{ok, Byte} -> receive_len(Socket, Len-1, Byte++RevBuffer);
		_ -> erlang:error(timeout)
	end.
%------------------------------------------------------------------------------
create_websocket_handler(Socket, HandlerParameter) ->
	create_websocket_handler(Socket, HandlerParameter, self()).
create_websocket_handler(Socket, HandlerParameter, Owner) ->
	HandlerPid = spawn(?MODULE, main_start, [Socket, Owner, HandlerParameter]),
	socket:controlling_process(Socket, HandlerPid),

	WebSocket = ?WS_FMT(HandlerPid),

	WebSocket.
%------------------------------------------------------------------------------
main_start(Socket, Owner, HandlerParameter) ->
	main_load(HandlerParameter),

	MailBox = queue:new(),
	Receiver = spawn_link(?MODULE, receiver_start, [Socket, self()]),

	main_loop(Socket, Owner, Receiver, MailBox).
%------------------------------------------------------------------------------
main_load([]) -> ok;
main_load([{Key, Value}|Parameter]) -> 
	put(Key, Value),
	main_load(Parameter);
main_load([_|Parameter]) ->
	main_load(Parameter).
%------------------------------------------------------------------------------
main_loop(Socket, Owner, Receiver, MailBox) ->
receive
	% WebSocket API
	?SEND_REQ(From, Data) ->
		Reply = send_frame(Socket, Data),
		From ! Reply,
		main_loop(Socket, Owner, Receiver, MailBox);
	?RECV_REQ(From, _Timeout) ->
		{Reply, NewMailBox} = recv_frame(MailBox),
		From ! Reply,
		main_loop(Socket, Owner, Receiver, NewMailBox);
	?CHANGE_OWNER(Owner, NewOwner) ->
		Owner ! ?CHANGE_OWNER_OK,
		main_loop(Socket, NewOwner, Receiver, MailBox);
	?CHANGE_OWNER(From, _) ->
		From ! ?CHANGE_OWNER_ERROR(not_owner),
		main_loop(Socket, Owner, Receiver, MailBox);
	?CLOSE_REQ ->
		socket:close(Socket),
		Owner ! ?WS_CLOSE_SIGNAL,
		main_end_loop(MailBox);
	% Receiver Update
	?RECV_NEW(Receiver, Msg) ->
		main_loop(Socket, Owner, Receiver, queue:in(Msg, MailBox));
	?RECV_CLOSE(Receiver) ->
		socket:close(Socket),
		Owner ! ?WS_CLOSE_SIGNAL,
		main_end_loop(MailBox);
	% Other
	X ->
		?print("main_loop", X),
		main_loop(Socket, Owner, Receiver, MailBox)
end.
%------------------------------------------------------------------------------
main_end_loop(MailBox) ->
receive
	?ACCEPT_REQ(From, _) ->
		From ! ?ACCEPT_RES_ERROR(closed),
		main_end_loop(MailBox);
	?RECV_REQ(From, _) ->
		case queue:out(MailBox) of
			{empty, NewMailBox} ->
				From ! ?RECV_RES_ERROR(closed);
			{{value, Reply}, NewMailBox} ->
				From ! ?RECV_RES_OK(Reply)
		end,
		main_end_loop(NewMailBox);
	?SEND_REQ(From, _) ->
		From !?SEND_RES_ERROR(closed),
		main_end_loop(MailBox);
	?CHANGE_OWNER(From, _) ->
		From ! {error, closed},
		main_end_loop(MailBox);
	_ ->
		main_end_loop(MailBox)
end.
%------------------------------------------------------------------------------
send_frame(Socket, Data) ->
	?FRAME_SUCESS(Frame) = hixie_frame:frame({text, Data}),
	case socket:send(Socket, Frame) of
		ok -> ?SEND_RES_OK;
		{error, Reason} -> ?SEND_RES_ERROR(Reason)
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
	case hixie_frame:unframe(Stream, Context) of
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
	case socket:listen(Mode, Port) of
		{ok, Listen} ->
			Owner ! {self(), ok},
			listen_loop(Listen, Owner);
		{error, Reason} ->
			Owner ! {self(), Reason}
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
	main_end_loop(queue:new()).
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
		{error, {error, Reason}} ->
			socket:send(Socket, ?RESPONSE_ERROR),
			socket:close(Socket),
			?ACCEPT_RES_ERROR(Reason);
		{'EXIT', {{case_clause, Reason}, _}} ->
			socket:send(Socket, ?RESPONSE_ERROR),
			socket:close(Socket),
			?ACCEPT_RES_ERROR(Reason);
		{'EXIT', {Reason, _}} ->
			socket:send(Socket, ?RESPONSE_ERROR),
			socket:close(Socket),
			?ACCEPT_RES_ERROR(Reason);
		WebSocket ->
			?ACCEPT_RES_OK(WebSocket)
	end,
	SocketOwner ! Reply.
%------------------------------------------------------------------------------
accept_request_1(Socket, SocketOwner) ->
	RequestHeader = receive_header(Socket),
	Key3 = receive_key3(Socket),

	Mode = socket:getmode(Socket),

	Request = ws_header:parse(RequestHeader ++ Key3),
	Response = hixie76_lib:gen_response(Request, Mode),

	SubProtocol = hixie76_lib:get_subprotocol(Response),

	ResponseHeader = ws_header:to_string(Response),
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
	HandlerParam = 
		[{request, Request},
		 {response, Response}, 
		 {subprotocol, SubProtocol}],
	create_websocket_handler(Socket, HandlerParam, SocketOwner).
%------------------------------------------------------------------------------

