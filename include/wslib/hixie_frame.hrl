%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Definir Flags dos Frames do Draft-Hixie
%% Criado: 05/26/11 11:11:56 (HPC)

%------------------------------------------------------------------------------
-define(FLAG_TXT, 16#00).
-define(FLAG_BIN, 16#80).
-define(FLAG_END, 16#ff).
%------------------------------------------------------------------------------
-define(TXT_LOW, ?FLAG_TXT).
-define(TXT_HIG, ?FLAG_BIN - 1).
-define(BIN_LOW, ?FLAG_BIN).
-define(BIN_HIG, ?FLAG_END).
%------------------------------------------------------------------------------
