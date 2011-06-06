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
-vsn(1).

-define(TODO, {error, todo}).

-export([connect/1, connect/2, listen/2]).
-export([accept/1, accept/2, accept/3, recv/1, recv/2, send/2, close/1]).
-export([geturl/1, getsubprotocol/1, getstate/1]).
%------------------------------------------------------------------------------
%% Cria um WebSocket a ser usado pelo Cliente
connect(_Url) ->
	?TODO.
connect(_Url, _Options) ->
	?TODO.

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

%% Recebe uma mensagem transmitida via WebSocket
recv(_WebSocket) ->
	recv(_WebSocket, infinity).
recv(_WebSocket, _Timeout) ->
	?TODO.

%% Envia uma mensagem via WebSocket
send(_WebSocket, {_Type, _Data}) ->
	?TODO.

%% Encerra uma conexao WebSocket
close(_WebSocket) ->
	?TODO.
%------------------------------------------------------------------------------
%% Obtem a Url utilizada para estabelecer a Conexao WebSocket
geturl(_WebSocket) ->
	?TODO.

%% Obtem o Subprotocolo definido durante o HandShake
getsubprotocol(_WebSocket) ->
	?TODO.

%% Obtem o estado atual do WebSocket
getstate(_WebSocket) ->
	?TODO.
%------------------------------------------------------------------------------
