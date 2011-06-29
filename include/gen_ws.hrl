%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 06/13/11 21:17:39 (HPC)

%------------------------------------------------------------------------------
% Default Settings
-define(DEF_ORIGIN, "127.0.0.1").
-define(DEF_CON_OPT, []).
-define(DEF_SUBP, []).
-define(DEF_TIMEOUT, infinity).
%------------------------------------------------------------------------------
% Magic Numbers
-define(ALL, 0).
-define(ONLY_ONE, 1).
-define(SOLUTION_LEN, 16).
-define(KEY3_LEN, 8).
-define(HEADER_TIMEOUT, 2000).
-define(MAX_HEADER_LEN, 8192). % Apache Default Limit
%------------------------------------------------------------------------------
% Magic Characters
-define(CR, [$\r]). % In a list because is a gen_tcp:recv return type
-define(LF, [$\n]). % In a list because is a gen_tcp:recv return type
%------------------------------------------------------------------------------
% Function Placeholder
-define(print(Text), io:format("~p~n", [Text])).
-define(print(Message, Text), io:format("~s:~p~n", [Message, Text])).
%------------------------------------------------------------------------------
% Data Placeholder
-define(TODO, {error, todo}).
-define(REPLY_ERROR, {error, invalid_response}).
%------------------------------------------------------------------------------
% Socket Representation
-define(WS_FMT(WebSocket), {websocket, draft_hixie76, WebSocket}).
-define(WSL_FMT(WebSocket), {websocket_listen, draft_hixie76, WebSocket}).
%------------------------------------------------------------------------------
% WebSocket Cliente API
-define(ACCEPT_REQ(From, Timeout), {From, accept, Timeout}).
-define(ACCEPT_REQ(Timeout), ?ACCEPT_REQ(self(), Timeout)).

-define(RECV_REQ(From, Timeout),  {From, recv, Timeout}).
-define(RECV_REQ(Timeout),  ?RECV_REQ(self(), Timeout)).

-define(SEND_REQ(From, Message),  {From, send, Message}).
-define(SEND_REQ(Message),  ?SEND_REQ(self(), Message)).

-define(CLOSE_REQ, close).

-define(CHANGE_OWNER(From, NewOwner), {From, change_owner, NewOwner}).
-define(CHANGE_OWNER(NewOwner), ?CHANGE_OWNER(self(), NewOwner)).
%------------------------------------------------------------------------------
% WebSocket Handler Process Messages to Cliente API
-define(ACCEPT_RES_OK(From, WebSocket), {From, accept_ok, WebSocket}).
-define(ACCEPT_RES_OK(WebSocket), ?ACCEPT_RES_OK(self(), WebSocket)).

-define(ACCEPT_RES_ERROR(From, Reason), {From, accept_error, Reason}).
-define(ACCEPT_RES_ERROR(Reason), ?ACCEPT_RES_ERROR(self(), Reason)).

-define(RECV_RES_OK(From, Data), {From, recv_ok, Data}).
-define(RECV_RES_OK(Data), ?RECV_RES_OK(self(), Data)).

-define(RECV_RES_ERROR(From, Reason), {From, recv_error, Reason}).
-define(RECV_RES_ERROR(Reason), ?RECV_RES_ERROR(self(), Reason)).

-define(SEND_RES_OK(From),    {From, send, ok}).
-define(SEND_RES_OK, ?SEND_RES_OK(self())).

-define(SEND_RES_ERROR(From, Reason), {From, send, {error, Reason}}).
-define(SEND_RES_ERROR(Reason), ?SEND_RES_ERROR(self(), Reason)).

-define(CHANGE_OWNER_OK(From), {From, change_owner, ok}).
-define(CHANGE_OWNER_OK, ?CHANGE_OWNER_OK(self())).

-define(CHANGE_OWNER_ERROR(From, Reason), {From, change_owner, {error, Reason}}).
-define(CHANGE_OWNER_ERROR(Reason), ?CHANGE_OWNER_ERROR(self(), Reason)).

%------------------------------------------------------------------------------
% WebSocket Active Messages
-define(WS_RECV_SIGNAL(Frame),  {ws, ?WS_FMT(self()), Frame}).
-define(WS_RECV_SIGNAL(WebSocket, Frame), {ws, WebSocket, Frame}).

-define(WS_ERROR_SIGNAL, {ws_error, ?WS_FMT(self()), Reason}).
-define(WS_ERROR_SIGNAL(WebSocket), {ws_error, WebSocket, Reason}).

-define(WS_CLOSE_SIGNAL, {ws_closed, ?WS_FMT(self())}).
-define(WS_CLOSE_SIGNAL(WebSocket), {ws_closed, WebSocket}).
%------------------------------------------------------------------------------
-define(RECV_NEW(From, Frame), {From, recv_frame, Frame}).
-define(RECV_NEW(Frame), ?RECV_NEW(self(), Frame)).

-define(RECV_CLOSE(From), {From, receiver_closed}).
-define(RECV_CLOSE, ?RECV_CLOSE(self())).
%------------------------------------------------------------------------------
-define(RESPONSE_ERROR,
	"HTTP/1.1 400 Bad Request\r\n" ++
	"Content-Type: text/plain; charset=utf-8\r\n" ++
	"Server: ErlangWebSocket1.0\r\n" ++
	"Content-Length: 26\r\n\r\nOnly WebSocket Connections").
%------------------------------------------------------------------------------
