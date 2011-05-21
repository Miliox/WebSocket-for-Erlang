%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Definicao das constantes existentes no cabecalho websocket
%% Criado: 05/18/11 22:56:32 (HPC)

%% Expressoes Regulares
-define(RE_HTTP_METHOD, "[A-Z]+").
-define(RE_HTTP_URI,   "/[^ ]*").
-define(RE_HTTP_STATUS, "[0-9]{3}").
-define(RE_HTTP_REASON, "[^$]+").
-define(RE_REQ, "^("++ ?RE_HTTP_METHOD ++") ("++ ?RE_HTTP_URI ++") HTTP/1.1$").
-define(RE_RES, "^HTTP/1.1 ("++ ?RE_HTTP_STATUS ++") ("++ ?RE_HTTP_REASON ++")$").

-define(RE_FIELD_NAME, "[^:]+").
-define(RE_FIELD_VALUE, "[^$]+").
-define(RE_FIELD, "^("++ ?RE_FIELD_NAME  ++"): ("++ ?RE_FIELD_VALUE ++")$").

-define(RE_LINES, "[\r\n]+").
-define(RE_LINES_OPT, [{return, list}]).
-define(RE_FIELD_OPT, [{capture, all_but_first, list}]).
-define(RE_RES_OPT, ?RE_FIELD_OPT).
-define(RE_REQ_OPT, ?RE_FIELD_OPT).

%% Http Like HandShake
-define(HOST,    "Host").
-define(UPGRADE, "Upgrade").
-define(CONNECTION, "Connection").

%% Hixie ??-75
-define(HIXIE_RQT_ORIGIN, "Origin").
-define(HIXIE_RSP_ORIGIN, "WebSocket-Origin").
-define(HIXIE_LOCAL,      "WebSocket-Location").
-define(HIXIE_PROTOCOL,   "WebSocket-Protocol").

%% Hixie 76
-define(HYBI00_KEY1, "Sec-WebSocket-Key1").
-define(HYBI00_KEY2, "Sec-WebSocket-Key2").

%% Definido
-define(WS_ORIGIN,   "Sec-WebSocket-Origin").
-define(WS_PROTOCOL, "Sec-WebSocket-Protocol").

%% IETF HyBi
-define(HYBI_ACCEPT,  "Sec-WebSocket-Accept").
-define(HYBI_KEY,     "Sec-WebSocket-Key").
-define(HYBI_LOCAL,   "Sec-WebSocket-Location").
-define(HYBI_VERSION, "Sec-WebSocket-Version").

