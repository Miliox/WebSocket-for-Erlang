%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Definicao das constantes existentes no cabecalho websocket
%% Criado: 05/18/11 22:56:32 (HPC)

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
