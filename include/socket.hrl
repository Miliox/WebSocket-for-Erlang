%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : <OBJETIVO>
%% Criado: 06/28/11 10:56:25 (HPC)

% SSL Messages
-define(SSL_CLOSED(SSLSocket),        {ssl_closed, SSLSocket}).
-define(SSL_DATA(SSLSocket, Data),    {ssl, SSLSocket, Data}).
-define(SSL_ERROR(SSLSocket, Reason), {ssl_error, SSLSocket, Reason}).

%TCP Messages
-define(TCP_CLOSED(TCPSocket),        {tcp_closed, TCPSocket}).
-define(TCP_DATA(TCPSocket, Data),    {tcp, TCPSocket, Data}).
-define(TCP_ERROR(TCPSocket, Reason), {tcp_error,  TCPSocket, Reason}).

%Socket Messages
-define(SOCKET_CLOSED(Socket),        {sock_closed, Socket}).
-define(SOCKET_DATA(Socket, Data),    {sock, Socket, Data}).
-define(SOCKET_ERROR(Socket, Reason), {sock_error,  Socket, Reason}).

% Socket Wrapper
-define(SOCKET_TCP(Socket), {tcp, Socket}).
-define(SOCKET_SSL(Socket), {tls, Socket}).

% Socket Configuration
-define(TCP_OPT, [list, {packet, raw}, {active, false}, {reuseaddr, true}]).
-define(SSL_OPT, 
	?TCP_OPT ++ [{certfile, "doc/certificate.pem"}, {keyfile, "doc/key.pem"}]).


