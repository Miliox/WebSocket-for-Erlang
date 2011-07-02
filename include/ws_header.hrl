%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Macros usadas em ws_header
%% Criado: 07/02/11 11:18:27 (HPC)

%------------------------------------------------------------------------------
-define(DEFINED_HEADER(Draft, Type), {ok, {Draft, Type}}).
-define(ERROR_HEADER, {error, header}).
%------------------------------------------------------------------------------
-define(FIRST, 1).
-define(SPACE, $ ).
-define(COLON, $:).
-define(SEPARATOR(Field), [?COLON|[?SPACE|Field]]).
%------------------------------------------------------------------------------
