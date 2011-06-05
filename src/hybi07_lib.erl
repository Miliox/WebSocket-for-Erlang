%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo especifico das operacoes do draft-hybi-07
%% Criado: 06/05/11 18:47:52 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(hybi07_lib).
-author("elmiliox@gmail.com").
-vsn(1).
%------------------------------------------------------------------------------
-define(GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").
%------------------------------------------------------------------------------
-include("ws_protocol_header.hrl").
%------------------------------------------------------------------------------
-import(base64).
-import(crypto).
-import(ws_header).
%------------------------------------------------------------------------------
-export([resolve_trial/1]).
%------------------------------------------------------------------------------
resolve_trial(Request) when is_list(Request) ->
	{found, Key} = ws_header:find(?HYBI_KEY, Request),
	
	binary_to_list( base64:encode( crypto:sha(Key ++ ?GUID) ) ).
%------------------------------------------------------------------------------
