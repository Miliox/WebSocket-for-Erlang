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
-include("hixie_frame.hrl").
-include("ws_frame.hrl").
%------------------------------------------------------------------------------
-export([frame/1, unframe/1, unframe/2]).
%------------------------------------------------------------------------------
frame({Type, BinData}) when is_binary(BinData) ->
	Data = binary_to_list(BinData),
	frame({Type, Data});
frame({text, Data}) when is_list(Data) ->
	Frame = [?FLAG_TXT] ++ Data ++ [?FLAG_END],
	?FRAME_SUCESS(Frame);
frame({binary, Data}) when is_list(Data) ->
	?FRAME_ERROR(todo);
frame(_) ->
	?FRAME_ERROR(badarg).
%------------------------------------------------------------------------------
unframe(BitStream) when is_binary(BitStream) ->
	unframe(binary_to_list(BitStream));
unframe([Flag|TailStream]=Stream) when is_list(Stream) ->
	case Flag of
		Txt when Txt >= ?TXT_LOW andalso Txt =< ?TXT_HIG ->
			unframe_text(Stream);
		Bin when Bin >= ?BIN_LOW orelse Bin =< ?BIN_HIG ->
			unframe_bin(Stream);
		?FLAG_END ->
			?UNFRAME_SUCESS(?FRAME_SIGN(?SIGN_CLOSE), TailStream);
		_ ->
			?UNFRAME_ERROR(badarg, Stream)
	end;
unframe(BadArg) ->
	?UNFRAME_ERROR(badarg, BadArg).
%------------------------------------------------------------------------------
unframe(BitStream, Context) when is_binary(BitStream) ->
	unframe(binary_to_list(BitStream), Context);
unframe(Stream, {Type, State}) when is_list(Stream) ->
	case Type of
		text ->
			unframe_text(Stream, State);
		binary ->
			unframe_bin(Stream, State);
		_ ->
			?UNFRAME_ERROR(badarg, Stream)
	end.
unframe(BadArg, _) ->
	?UNFRAME_ERROR(badarg, BadArg).
%------------------------------------------------------------------------------
unframe_text([Flag|Stream]) 
when 
	(Flag >= ?TXT_LOW) andalso 
	(Flag =< ?TXT_HIG) ->
		unframe_text(Stream, []);
unframe_text(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
unframe_text([], Buffer) ->
	?UNFRAME_PARTIAL(?FRAME_TXT(Buffer), []);
unframe_text([?FLAG_END|Stream], Buffer) ->
	Text = lists:reverse(Buffer),
	?UNFRAME_SUCESS(?FRAME_TXT(Text), Stream);
unframe_text([Char|Stream], Buffer) ->
	unframe_text(Stream, [Char|Buffer]).
%------------------------------------------------------------------------------
unframe_bin(_) ->
	erlang:error(badarg).
unframe_bin(_, _) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
