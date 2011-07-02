%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Manipular mensagens em frames. Hixie 75 e 76 
%%            possuem o mesmo tipo de frame.
%% Criado: 05/26/11 10:46:23 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011
%------------------------------------------------------------------------------
-module(wslib.hixie_frame).
-author("elmiliox@gmail.com").
-vsn(2).
%------------------------------------------------------------------------------
-include("hixie_frame.hrl").
-include("ws_frame.hrl").
%------------------------------------------------------------------------------
-import(lists).
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
unframe([Flag|_]=Stream) when is_list(Stream) ->
	case Flag of
		Txt when Txt >= ?TXT_LOW andalso Txt =< ?TXT_HIG ->
			unframe_text(Stream);
		Bin when Bin >= ?BIN_LOW orelse Bin =< ?BIN_HIG ->
			unframe_bin(Stream);
		_ ->
			?UNFRAME_ERROR(badarg, Stream)
	end;
unframe(BadArg) ->
	?UNFRAME_ERROR(badarg, BadArg).
%------------------------------------------------------------------------------
unframe(Stream, nil) ->
	unframe(Stream);
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
	end;
unframe(BadArg, _) ->
	?UNFRAME_ERROR(badarg, BadArg).
%------------------------------------------------------------------------------
unframe_text([Flag|Stream]) 
when 
	(Flag >= ?TXT_LOW) andalso 
	(Flag =< ?TXT_HIG) ->
		unframe_text(Stream, []);
unframe_text(Stream) ->
	?UNFRAME_ERROR(badarg, Stream).
%------------------------------------------------------------------------------
unframe_text([], Buffer) ->
	?UNFRAME_PARTIAL(?FRAME_TXT(Buffer), []);
unframe_text([?FLAG_END|Stream], Buffer) ->
	Text = lists:reverse(Buffer),
	?UNFRAME_SUCESS(?FRAME_TXT(Text), Stream);
unframe_text([Char|Stream], Buffer) ->
	unframe_text(Stream, [Char|Buffer]).
%------------------------------------------------------------------------------
unframe_bin([InitialFlag|Stream]) 
when 
	(InitialFlag >= ?BIN_LOW) andalso 
	(InitialFlag =< ?BIN_HIG) ->
		Factor = InitialFlag - ?BIN_LOW,
		[H|T] = Stream,
		case 
			InitialFlag == 16#ff andalso 
			H == 16#00
		of
			true ->
				?UNFRAME_SUCESS(
					?FRAME_SIGN(?SIGN_CLOSE), T);
			false ->
				unframe_bin(Stream, Factor)
		end.
%------------------------------------------------------------------------------
unframe_bin([Byte|Stream], AccLength) 
when
	(Byte >= ?BIN_LOW) andalso 
	(Byte =< ?BIN_HIG) ->
		Factor = Byte - ?BIN_LOW,
		Length = (AccLength * 128) + Factor,

		unframe_bin(Stream, Length);
unframe_bin([EndFlag|Stream], Length) 
when 
	EndFlag < ?BIN_LOW andalso
	EndFlag >= 16#00 ->

		discard_payload(Stream, Length).
%------------------------------------------------------------------------------
discard_payload(Stream, 0) ->
	?UNFRAME_SUCESS(?FRAME_BIN([]), Stream);
discard_payload([_|T], Len) ->
	discard_payload(T, Len-1).
%------------------------------------------------------------------------------
