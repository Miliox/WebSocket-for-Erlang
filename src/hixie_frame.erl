%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Manipular mensagens em frames. Hixie 75 e 76 
%%            possuem o mesmo tipo de frame.
%% Criado: 05/26/11 10:46:23 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(hixie_frame).
-author("elmiliox@gmail.com").
-vsn(1).

%------------------------------------------------------------------------------
-include("ws_frame.hrl").
-include("hixie_frame.hrl").
%------------------------------------------------------------------------------
-export([frame/1, unframe/1]).
%------------------------------------------------------------------------------
frame({Type, BinData}) when is_binary(BinData) ->
	Data = binary_to_list(BinData),
	frame({Type, Data});
frame({text, Data}) when is_list(Data) ->
	Frame = [?FLAG_TXT] ++ Data ++ [?FLAG_END],
	{ok, Frame};
frame({binary, Data}) when is_list(Data) ->
	{error, not_supported};
frame(_) ->
	{error, badarg}.
%------------------------------------------------------------------------------
unframe(BitStream) when is_binary(BitStream) ->
	unframe(binary_to_list(BitStream));
unframe(Stream) when is_list(Stream) ->
	[Flag|Tail] = Stream,
	case Flag of
		Txt when Txt >= ?TXT_LOW andalso Txt =< ?TXT_HIG ->
			unframe_text(Stream);
		Bin when Bin >= ?BIN_LOW orelse Bin =< ?BIN_HIG ->
			unframe_bin(Stream);
		?FLAG_END ->
			{ok, ?FRAME_SIGN(?SIGN_CLOSE), Tail};
		_ ->
			{error, badarg, Stream}
	end;
unframe(Unknown) ->
	{error, badarg, Unknown}.
%------------------------------------------------------------------------------
unframe_text([Flag|Tail]) 
when 
	(Flag >= ?TXT_LOW)  andalso 
	(Flag =< ?TXT_HIG) ->
		unframe_text(Tail, []);
unframe_text(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
unframe_text([], Buffer) ->
	{incomplete, {text, Buffer}, []};
unframe_text([?FLAG_END|Tail], Buffer) ->
	Text = lists:reverse(Buffer),
	{ok, ?FRAME_TXT(Text), Tail};
unframe_text([Char|Tail], Buffer) ->
	unframe_text(Tail, [Char|Buffer]).
%------------------------------------------------------------------------------
unframe_bin(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
