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
-vsn(2).
%------------------------------------------------------------------------------
-include("socket.hrl").
%------------------------------------------------------------------------------
-import(ssl).
-import(gen_tcp).
%------------------------------------------------------------------------------
-export([accept/2, close/1, listen/2, recv/2, recv/3, send/2]).
-export([controlling_process/2, getmode/1]).
%------------------------------------------------------------------------------
accept(?SOCKET_TCP(Socket), Timeout) ->
	wrap_tcp(gen_tcp:accept(Socket, Timeout));
accept(?SOCKET_SSL(Socket), Timeout) ->
	Accept = 
	case ssl:transport_accept(Socket, Timeout) of
		{ok, SSLSocket}=Ok -> 
			case ssl:ssl_accept(SSLSocket, Timeout) of
				ok -> Ok;
				Error -> Error
			end;
		Error -> Error
	end,
	wrap_ssl(Accept).
%------------------------------------------------------------------------------
close(?SOCKET_TCP(Socket)) ->
	gen_tcp:close(Socket);
close(?SOCKET_SSL(Socket)) ->
	ssl:close(Socket).
%------------------------------------------------------------------------------
connect(Mode, Address, Port, Timeout) ->
	case Mode of
		normal ->
			TCP = gen_tcp:connect(Address, Port, ?TCP_OPT, Timeout),
			wrap_tcp(TCP);
		secure ->
			ssl:start(),
			SSL = ssl:connect(Address, Port, ?TCP_OPT, Timeout),
			wrap_ssl(SSL)
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
listen(normal, Port) ->
	wrap_tcp(gen_tcp:listen(Port, ?TCP_OPT));
listen(secure, Port) ->
	ssl:start(),
	wrap_ssl(ssl:listen(Port, ?SSL_OPT));
listen(_, _) ->
	{error, badarg}.
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
getmode(?SOCKET_TCP(_)) ->
	normal;
getmode(?SOCKET_SSL(_)) ->
	secure.
