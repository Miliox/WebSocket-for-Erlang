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
-vsn(2).
%------------------------------------------------------------------------------
-include("gen_ws.hrl").
-include("ws_frame.hrl").
%------------------------------------------------------------------------------
-import(gen_tcp).
-import(hixie76_lib).
-import(ws_url).
%------------------------------------------------------------------------------
-export([connect/1, connect/2, listen/2]).
-export([accept/1, accept/2, accept/3, recv/1, recv/2, send/2, close/1]).
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
get_default(subprotocol) -> ?DEF_SUBP;
get_default(timeout) -> ?DEF_TIMEOUT;
get_default(_) -> erlang:error(badarg).
%------------------------------------------------------------------------------
%% Cria um WebSocket a ser usado pelo Servidor
listen(_Port, _Options) ->
	?TODO.
%------------------------------------------------------------------------------
%% Efetua o HandShake e estabelece uma Conexao WebSocket
accept(_ListenWebSocket) ->
	?TODO.
accept(_ListenWebSocket, _HandShakeOptions) ->
	accept(_ListenWebSocket, _HandShakeOptions, infinity).
accept(_ListenWebSocket, _HandShakeOptions, _Timeout) ->
	?TODO.
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
send(?WS_FMT(Handler), {text, Data}) ->
	Handler ! ?SEND_REQ(Data),
	receive
		?SEND_RES_OK(Handler) -> ok;
		?SEND_RES_ERROR(Handler, Reason) -> 
			{error, Reason}
		after 2000 ->
			{error, timeout}
	end.
%------------------------------------------------------------------------------
%% Encerra uma conexao WebSocket
close(?WS_FMT(Handler)) ->
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
	ResponseHeader = receive_response_header(TCPSocket),
	ServerSolution = receive_response_solution(TCPSocket),

	Response = ws_header:parse(ResponseHeader ++ ServerSolution),
	SubProtocol = hixie76_lib:get_subprotocol(Response),

	case ServerSolution == Answer of
		true ->
			create_websocket_handler(
				TCPSocket, 
				HandlerParameter ++ 
					[{response, Response}, 
					 {subprotocol, SubProtocol}]);
		false ->
			?REPLY_ERROR
	end.	
%------------------------------------------------------------------------------
receive_response_header(TCPSocket) ->
	receive_response_header_loop(TCPSocket, []).
%------------------------------------------------------------------------------
% Header Loop Termina quanto encontrar CRLFCRLF
receive_response_header_loop(TCPSocket, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE) of
		{ok, ?CR} ->
			receive_response_header_loop_1(
				TCPSocket, ?CR ++ RevBuffer);
		{ok, Char} ->
			receive_response_header_loop(
				TCPSocket, Char ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
% Falta LFCRLF
receive_response_header_loop_1(TCPSocket, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE) of
		{ok, ?LF} ->
			receive_response_header_loop_2(
				TCPSocket, ?LF ++ RevBuffer);
		{ok, Char} ->
			receive_response_header_loop(
				TCPSocket, Char ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
% Falta CRLF
receive_response_header_loop_2(TCPSocket, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE) of
		{ok, ?CR} ->
			receive_response_header_loop_3(
				TCPSocket, ?CR ++ RevBuffer);
		{ok, Char} ->
			receive_response_header_loop(
				TCPSocket, Char ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
% Falta LF
receive_response_header_loop_3(TCPSocket, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE) of
		{ok, ?LF} ->
			lists:reverse(?LF ++ RevBuffer);
		{ok, Char} ->
			receive_response_header_loop(
				TCPSocket, Char ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
receive_response_solution(TCPSocket) ->
	receive_response_solution(TCPSocket, ?SOLUTION_LEN, []).
%------------------------------------------------------------------------------
receive_response_solution(_, Len, RevBuffer) when Len =< 0 ->
	lists:reverse(RevBuffer);
receive_response_solution(TCPSocket, Len, RevBuffer) ->
	case gen_tcp:recv(TCPSocket, ?ONLY_ONE) of
		{ok, Byte} -> receive_response_solution(
				TCPSocket, Len-1, Byte ++ RevBuffer)
	end.
%------------------------------------------------------------------------------
create_websocket_handler(TCPSocket, HandlerParameter) ->
	HandlerPid = spawn(?MODULE, main_start, [TCPSocket, self(), HandlerParameter]),
	gen_tcp:controlling_process(TCPSocket, HandlerPid),

	WebSocket = ?WS_FMT(HandlerPid),

	{ok, WebSocket}.
%------------------------------------------------------------------------------
main_start(TCPSocket, Owner, HandlerParameter) ->
	main_load_param(HandlerParameter),

	MailBox = queue:new(),
	Receiver = spawn_link(?MODULE, recv_start, [TCPSocket, self()]),

	main_loop(TCPSocket, Owner, Receiver, MailBox).
%------------------------------------------------------------------------------
main_load_param([]) -> ok;
main_load_param([{Key, Value}|Parameter]) -> 
	put(Key, Value),
	main_load_param(Parameter).

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
		?CHANGE_OWNER(_, _) ->
			Owner ! ?CHANGE_OWNER_ERROR(not_owner),
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
recv_start(TCPSocket, Handler) ->
	recv_loop(TCPSocket, Handler, []).
%------------------------------------------------------------------------------
recv_loop(TCPSocket, Handler, Buffer) ->
	case gen_tcp:recv(TCPSocket, ?ALL) of
		{ok, Stream} ->
			?UNFRAME_SUCESS(Frame, _) = hixie_frame:unframe(Stream),
			Handler ! ?RECV_NEW(Frame),
			recv_loop(TCPSocket, Handler, Buffer);
		{error, closed} ->
			Handler ! ?RECV_CLOSE;
		X ->
			?print("rcv_loop", X),
			recv_loop(TCPSocket, Handler, Buffer)
	end.
%------------------------------------------------------------------------------
