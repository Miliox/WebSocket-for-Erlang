%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Definicao das Macros de manipulacao cabecalho websocket
%% Criado: 05/18/11 22:56:32 (HPC)

% RE Options
-define(RE_LINES_OPT, [{return, list}]).
-define(RE_FIELD_OPT, [{capture, all_but_first, list}]).
-define(RE_RES_OPT, ?RE_FIELD_OPT).
-define(RE_REQ_OPT, ?RE_FIELD_OPT).
%------------------------------------------------------------------------------
%% Elements Regex
-define(RE_HTTP_METHOD, "[A-Z]+").
-define(RE_HTTP_REASON, "[^$]+").
-define(RE_HTTP_STATUS, "[0-9]{3}").
-define(RE_HTTP_URI,    "/[^ ]*").
-define(RE_FIELD_NAME,  "[^:]+").
-define(RE_FIELD_VALUE, "[^$]+").
%------------------------------------------------------------------------------
-ifdef(NO_MP).
% Start Line-------------------------------------------------------------------
-define(RE_RES, "^HTTP/1.1 ("++?RE_HTTP_STATUS++") ("++?RE_HTTP_REASON++")$").
-define(RE_REQ, "^("++?RE_HTTP_METHOD++") ("++?RE_HTTP_URI++") HTTP/1.1$").
% Break Line-------------------------------------------------------------------
-define(RE_LINES, "\r\n").
% Field------------------------------------------------------------------------
-define(RE_FIELD, "^("++ ?RE_FIELD_NAME  ++"): ("++ ?RE_FIELD_VALUE ++")$").
%------------------------------------------------------------------------------
-else. %NO_MP
% Start Line-------------------------------------------------------------------
-define(RE_RES,{re_pattern,2,0,
<<69,82,67,80,124,0,0,0,16,0,0,0,1,0,0,0,2,0,0,0,0,0,0,0,40,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,93,0,80,25,27,72,27,84,27,84,27,80,27,47,27,49,12,27,49,27,32,94,
0,43,0,1,77,0,0,0,0,0,0,255,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
75,0,3,0,3,84,0,43,27,32,94,0,7,0,2,45,36,84,0,7,26,84,0,80,0>>}).

-define(RE_REQ, {re_pattern,2,0,
<<69,82,67,80,121,0,0,0,16,0,0,0,5,0,0,0,2,0,0,0,0,0,49,2,40,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,93,0,77,25,94,0,39,0,1,77,0,0,0,0,0,0,0,0,254,255,255,7,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,71,84,0,39,27,32,94,0,9,0,2,27,47,43,32,84,
0,9,27,32,27,72,27,84,27,84,27,80,27,47,27,49,12,27,49,84,0,77,0>>}).
% Break Line-------------------------------------------------------------------
-define(RE_LINES, {re_pattern,0,0,
<<69,82,67,80,51,0,0,0,0,0,0,0,38,0,0,0,0,0,0,0,13,0,10,0,40,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,93,0,7,27,13,27,10,84,0,7,0>>}).
% Field------------------------------------------------------------------------
-define(RE_FIELD, {re_pattern,2,0,
<<69,82,67,80,73,0,0,0,16,0,0,0,5,0,0,0,2,0,0,0,0,0,32,2,40,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,93,0,29,25,94,0,7,0,1,45,58,84,0,7,27,58,27,32,94,0,7,0,2,45,36,
84,0,7,26,84,0,29,0>>}).
%------------------------------------------------------------------------------
-endif. % NO_MP
