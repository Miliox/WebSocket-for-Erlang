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
-define(DECODE_KEY_LEN, 16).
-define(GUID, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").
%------------------------------------------------------------------------------
-include("data_size.hrl").
-include("ws_protocol_header.hrl").
%------------------------------------------------------------------------------
-import(base64).
-import(crypto).
-import(ws_header).
%------------------------------------------------------------------------------
-export([make_trial/0, resolve_trial/1, random_key/0]).
%------------------------------------------------------------------------------
resolve_trial(Request) when is_list(Request) ->
	{found, Key} = ws_header:find(?HYBI_KEY, Request),
	resolve_trial_from_key(Key).
%------------------------------------------------------------------------------
resolve_trial_from_key(Key) ->
	binary_to_list( base64:encode( crypto:sha(Key ++ ?GUID) ) ).
%------------------------------------------------------------------------------
make_trial() ->
	Key = random_key(),
	Answer = resolve_trial_from_key(Key),
	{Key, Answer}.
%------------------------------------------------------------------------------
random_key() ->
	random_key_1([], ?DECODE_KEY_LEN).
%------------------------------------------------------------------------------
random_key_1(DecodeKey, 0) ->
	binary_to_list(base64:encode(DecodeKey));
random_key_1(DecodeKey, Len) ->
	Char = random:uniform(?BYTE),
	random_key_1([Char|DecodeKey], Len-1).
%------------------------------------------------------------------------------
