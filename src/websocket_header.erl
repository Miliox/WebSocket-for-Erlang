%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo que abstrai as operacoes sobre o cabecalho WebSocket
%% Criado: 05/18/11 22:28:39 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(websocket_header).
-author("elmiliox@gmail.com").
-vsn(1).

-export([parse/1]).

%------------------------------------------------------------------------------
parse(WebSocketHeader) when is_binary(WebSocketHeader) ->
	parse(binary_to_list(WebSocketHeader)).
parse(WebSocketHeader) ->
	BinLines = re:split(WebSocketHeader, "[\r\n]+"),
	Bin2List = fun(Bin) -> binary_to_list(Bin) end,

	Lines = lists:map(Bin2List, BinTokens),
	
	map_to_key_value(Lines).
%------------------------------------------------------------------------------
