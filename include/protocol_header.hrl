%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno : Emiliano Carlos de Moraes Firmino ( elmiliox@gmail.com )
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Definicao das macro do protocolo relacionado ao cabecalho
%% Criado: 05/21/11 22:56:32 (HPC)

%------------------------------------------------------------------------------
-define(HIXIE75, draft_hixie_75).
-define(HIXIE76, draft_hixie_76).
-define(HYBI,    draft_hybi_07).
%------------------------------------------------------------------------------
-define(WS_PREFIX,     "WebSocket-").
-define(WS_SEC_PREFIX, "Sec-" ++ ?WS_PREFIX).
%% Http Like HandShake
% Start Line
-define(WS_HOST,       "Host").
-define(WS_MET_VAL,    "GET").
-define(WS_REASON_VAL, " Protocol Handshake").
% Header Field
-define(WS_UPGRADE,    "Upgrade").
-define(WS_UPG_VAL,    "WebSocket").
-define(WS_CONNECTION, "Connection").
-define(WS_CON_VAL,    ?WS_UPGRADE).
-define(WS_ORIGIN,     "Origin").
-define(WS_LOCATION,   "Location").
-define(WS_PROTOCOL,   "Protocol").
% Pseudo Field
-define(WS_URI,    path).
-define(WS_METHOD, method).
-define(WS_STATUS_CODE,   status).
-define(WS_REASON_PHRASE,  reason).
%------------------------------------------------------------------------------
%% Hixie 66-75
% Start Line
-define(HIXIE75_MET_VAL,    ?WS_MET_VAL).
-define(HIXIE75_HOST,       ?WS_HOST).
-define(HIXIE75_REASON_VAL, "Web Socket" ++ ?WS_REASON_VAL).
% Header * Field
-define(HIXIE75_UPGRADE,    ?WS_UPGRADE).
-define(HIXIE75_UPG_VAL,    ?WS_UPG_VAL).
-define(HIXIE75_CONNECTION, ?WS_CONNECTION).
-define(HIXIE75_CON_VAL,    ?WS_CON_VAL).
-define(HIXIE75_PROTOCOL,   ?WS_PREFIX ++ ?WS_PROTOCOL). % optional field
% Header Request Field
-define(HIXIE75_ORIGIN_REQ, ?WS_ORIGIN).
% Header Response Field
-define(HIXIE75_ORIGIN_RES, ?WS_PREFIX ++ ?WS_ORIGIN).
-define(HIXIE75_LOCATION,   ?WS_PREFIX ++ ?WS_LOCATION).
% Pseudo Field
-define(HIXIE75_URI,    ?WS_URI).
-define(HIXIE75_METHOD, ?WS_METHOD).
-define(HIXIE75_STATUS_CODE,   ?WS_STATUS_CODE).
-define(HIXIE75_REASON_PHRASE, ?WS_REASON_PHRASE).
%------------------------------------------------------------------------------
%% Hixie 76 and HYBI00
% Start Line
-define(HIXIE76_MET_VAL,    ?HIXIE75_MET_VAL).
-define(HIXIE76_HOST,       ?HIXIE75_HOST).
-define(HIXIE76_REASON_VAL, "WebSocket" ++ ?WS_REASON_VAL).
% Header * Field
-define(HIXIE76_UPGRADE,    ?HIXIE75_UPGRADE).
-define(HIXIE76_UPG_VAL,    ?HIXIE75_UPG_VAL).
-define(HIXIE76_CONNECTION, ?HIXIE75_CONNECTION).
-define(HIXIE76_CON_VAL,    ?HIXIE75_CON_VAL).
-define(HIXIE76_PROTOCOL,   ?WS_SEC_PREFIX ++ ?WS_PROTOCOL). % optional field
% Header Request Field
-define(HIXIE76_ORIGIN_REQ, ?HIXIE75_ORIGIN_REQ).
-define(HIXIE76_KEY1, ?WS_SEC_PREFIX ++ "Key1").
-define(HIXIE76_KEY2, ?WS_SEC_PREFIX ++ "Key2").
% Header Response Field
-define(HIXIE76_ORIGIN_RES, ?WS_SEC_PREFIX ++ ?WS_ORIGIN).
-define(HIXIE76_LOCATION,   ?WS_SEC_PREFIX ++ ?WS_LOCATION).
% Pseudo Field
-define(HIXIE76_URI,    ?HIXIE75_URI).
-define(HIXIE76_METHOD, ?HIXIE75_METHOD).
-define(HIXIE76_STATUS_CODE,   ?HIXIE75_STATUS_CODE).
-define(HIXIE76_REASON_PHRASE, ?HIXIE75_REASON_PHRASE).
%------------------------------------------------------------------------------
%% IETF HyBi 01-07
% Start Line
-define(HYBI_MET_VAL,     ?HIXIE76_MET_VAL).
-define(HYBI_HOST,        ?HIXIE76_HOST).
-define(HYBI_REASON_VAL, "Switching Protocols").
% Header * Field
-define(HYBI_UPGRADE,    ?HIXIE76_UPGRADE).
-define(HYBI_UPG_VAL,    "websocket").
-define(HYBI_CONNECTION, ?HIXIE76_CONNECTION).
-define(HYBI_CON_VAL,    ?HIXIE76_CON_VAL).
-define(HYBI_PROTOCOL,   ?WS_SEC_PREFIX ++ ?WS_PROTOCOL). % optional field
% Header Request Field
-define(HYBI_ORIGIN,  ?WS_SEC_PREFIX ++ ?WS_ORIGIN).
-define(HYBI_VERSION, ?WS_SEC_PREFIX ++ "Version").
-define(HYBI_KEY,     ?WS_SEC_PREFIX ++ "Key").
% Header Response Field
-define(HYBI_ACCEPT,  ?WS_SEC_PREFIX ++ "Accept").
% Pseudo Field
-define(HYBI_URI,    ?HIXIE76_URI).
-define(HYBI_METHOD, ?HIXIE76_METHOD).
-define(HYBI_STATUS_CODE,   ?HIXIE76_STATUS_CODE).
-define(HYBI_REASON_PHRASE, ?HIXIE76_REASON_PHRASE).

