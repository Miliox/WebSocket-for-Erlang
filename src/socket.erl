%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Wrapper para TCP e SSL
%% Criado: 06/28/11 10:42:44 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(socket).
-author("elmiliox@gmail.com").
-vsn(1).
%------------------------------------------------------------------------------
-include("socket.hrl").
%------------------------------------------------------------------------------
-import(ssl).
-import(gen_tcp).
%------------------------------------------------------------------------------
-export([accept/2, close/1, listen/1, recv/2, recv/3, send/2]).
-export([controlling_process/2]).
%------------------------------------------------------------------------------
accept(?SOCKET_TCP(Socket), Timeout) ->
	wrap_tcp(gen_tcp:accept(Socket, Timeout));
accept(?SOCKET_SSL(_Socket), _Timeout) ->
	erlang:throw(todo).
	%wrap_ssl(ssl:accept(Socket, Timeout)).
%------------------------------------------------------------------------------
close(?SOCKET_TCP(Socket)) ->
	gen_tcp:close(Socket);
close(?SOCKET_SSL(Socket)) ->
	ssl:close(Socket).
%------------------------------------------------------------------------------
connect(Mode, Address, Port, Timeout) ->
	case Mode of
		normal ->
			wrap_tcp(gen_tcp:connect(
					Address, Port, ?TCP_OPT, Timeout));
		secure ->
			ssl:start(),
			wrap_ssl(ssl:connect(
					Address, Port, ?TCP_OPT, Timeout))
	end.
%------------------------------------------------------------------------------
wrap_tcp({ok, TCPSocket}) ->
	{ok, ?SOCKET_TCP(TCPSocket)};
wrap_tcp(Error) ->
	Error.
%------------------------------------------------------------------------------
wrap_ssl({ok, SSLSocket}) ->
	{ok, ?SOCKET_SSL(SSLSocket)};
wrap_ssl(Error) ->
	Error.
%------------------------------------------------------------------------------
listen(?SOCKET_TCP(Listen)) ->
	gen_tcp:listen(Listen, ?TCP_OPT);
listen(?SOCKET_SSL(Listen)) ->
	ssl:start(),
	ssl:listen(Listen, ?TCP_OPT).
%------------------------------------------------------------------------------
recv(Socket, Length) ->
	recv(Socket, Length, infinity).
%------------------------------------------------------------------------------
recv(?SOCKET_TCP(Socket), Length, Timeout) ->
	gen_tcp:recv(Socket, Length, Timeout);
recv(?SOCKET_SSL(Socket), Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).
%------------------------------------------------------------------------------
send(?SOCKET_TCP(Socket), Data) ->
	gen_tcp:send(Socket, Data);
send(?SOCKET_SSL(Socket), Data) ->
	ssl:send(Socket, Data).
%------------------------------------------------------------------------------
controlling_process(?SOCKET_TCP(Socket), NewOwner) ->
	gen_tcp:controlling_process(Socket, NewOwner);
controlling_process(?SOCKET_SSL(Socket), NewOwner) ->
	ssl:controlling_process(Socket, NewOwner).
%------------------------------------------------------------------------------
