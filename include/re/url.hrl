%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Separar as macros  aplicadas em ws_url.erl do codigo.
%% Criado: 05/26/11 00:17:22 (HPC)

%------------------------------------------------------------------------------
-define(RE_WS_OPT, [{capture, all_but_first, list}]).

%------------------------------------------------------------------------------
-ifdef(NO_MP).
%------------------------------------------------------------------------------
-define(RE_WS_URL1, "(ws|wss)://([\\w\\d\.]+):([0-9]+)(/[^$]*)").
-define(RE_WS_URL2, "(ws|wss)://([\\w\\d\.]+)(/[^$]*)").
-define(RE_WS_URL3, "(ws|wss)://([\\w\\d\.]+):([0-9]+)").
-define(RE_WS_URL4, "(ws|wss)://([\\w\\d\.]+)").
%------------------------------------------------------------------------------
-else. % NO_MP
%------------------------------------------------------------------------------
-define(RE_WS_URL1, % Compiled Version of RE_WS_URL1
	{re_pattern,4,0,
	<<69,82,67,80,172,0,0,0,0,0,0,0,7,0,0,0,4,0,0,0,119,0,47,2,40,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,93,0,128,94,0,9,0,1,27,119,27,115,83,0,9,
	27,119,27,115,27,115,84,0,18,27,58,27,47,27,47,94,0,39,0,2,77,0,0,
	0,0,0,64,255,3,254,255,255,135,254,255,255,7,0,0,0,0,0,4,32,4,255,
	255,127,255,255,255,127,255,71,84,0,39,27,58,94,0,39,0,3,77,0,0,0,
	0,0,0,255,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,71,84,
	0,39,94,0,9,0,4,27,47,43,36,84,0,9,84,0,128,0>>}).
%------------------------------------------------------------------------------
-define(RE_WS_URL2, % Compiled Version of RE_WS_URL2
	{re_pattern,3,0,
	<<69,82,67,80,128,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,119,0,47,2,40,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,93,0,84,94,0,9,0,1,27,119,27,115,83,0,9,
	27,119,27,115,27,115,84,0,18,27,58,27,47,27,47,94,0,39,0,2,77,0,0,
	0,0,0,64,255,3,254,255,255,135,254,255,255,7,0,0,0,0,0,4,32,4,255,
	255,127,255,255,255,127,255,71,84,0,39,94,0,9,0,3,27,47,43,36,84,0,
	9,84,0,84,0>>}).
%------------------------------------------------------------------------------
-define(RE_WS_URL3,
	{re_pattern,3,0,
	<<69,82,67,80,160,0,0,0,0,0,0,0,7,0,0,0,3,0,0,0,119,0,58,2,40,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,93,0,116,94,0,9,0,1,27,119,27,115,83,0,9,27,119,
	27,115,27,115,84,0,18,27,58,27,47,27,47,94,0,39,0,2,77,0,0,0,0,0,64,255,
	3,254,255,255,135,254,255,255,7,0,0,0,0,0,4,32,4,255,255,127,255,255,255,
	127,255,71,84,0,39,27,58,94,0,39,0,3,77,0,0,0,0,0,0,255,3,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,71,84,0,39,84,0,116,0>>}).
%------------------------------------------------------------------------------
-define(RE_WS_URL4,
	{re_pattern,2,0,<<69,82,67,80,116,0,0,0,0,0,0,0,7,0,0,0,2,0,0,0,119,0,47,
	0,40,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,93,0,72,94,0,9,0,1,27,119,27,115,83,0,
	9,27,119,27,115,27,115,84,0,18,27,58,27,47,27,47,94,0,39,0,2,77,0,0,0,0,0,
	64,255,3,254,255,255,135,254,255,255,7,0,0,0,0,0,4,32,4,255,255,127,255,255,
	255,127,255,71,84,0,39,84,0,72,0>>}).
%------------------------------------------------------------------------------
-endif. % NO_MP
