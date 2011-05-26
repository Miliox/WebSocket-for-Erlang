%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : unificar as definicoes de formato de dado frame websocket
%% Criado: 05/26/11 11:05:33 (HPC)

%------------------------------------------------------------------------------
-define(FRAME_FMT(Type, Data), {Type, Data}).
%------------------------------------------------------------------------------
-define(FRAME_BIN(Bin),   ?FRAME_FMT(binary, Bin) ).
-define(FRAME_SIGN(Sign), ?FRAME_FMT(sign,  Sign) ).
-define(FRAME_TXT(Text),  ?FRAME_FMT(text,  Text) ).
%------------------------------------------------------------------------------
-define(SIGN_CLOSE, close).
%------------------------------------------------------------------------------
-define(FRAME_SUCESS(Frame), {ok,    Frame}).
-define(FRAME_ERROR(Reason), {error, Reason}).
%------------------------------------------------------------------------------
-define(UNFRAME_SUCESS(Frame, Stream),   {ok,      Frame,   Stream}).
-define(UNFRAME_ERROR(Reason, Stream),   {error,   Reason,  Stream}).
-define(UNFRAME_PARTIAL(Context,Stream), {partial, Context, Stream}).
%------------------------------------------------------------------------------
