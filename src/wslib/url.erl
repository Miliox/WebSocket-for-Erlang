%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo especifico das operacoes sobre WsUrl
%% Criado: 05/25/11 22:40:13 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011
%------------------------------------------------------------------------------
-module(wslib.url).
-author("elmiliox@gmail.com").
-vsn(2).
%------------------------------------------------------------------------------
-include("ws_url.hrl").
-include("ws_re_url.hrl").
%------------------------------------------------------------------------------
-import(re).
-import(erlang).
%------------------------------------------------------------------------------
-export([scheme/1, parse/1, to_string/1]).
%------------------------------------------------------------------------------
% Full Url
parse(Url) when is_list(Url) ->
	case re:run(Url, ?RE_WS_URL1, ?RE_WS_OPT) of
		{match, ["ws", Domain, Port, Path]} ->
			Host = Domain++":"++Port,
			{normal, Domain, Host, list_to_integer(Port), Path};
		{match, ["wss", Domain, Port, Path]} ->
			Host = Domain++":"++Port,
			{secure, Domain, Host, list_to_integer(Port), Path};
		nomatch ->
			parse_1(Url)
	end;
parse(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
% Url without Port
parse_1(Url) ->
	case re:run(Url, ?RE_WS_URL2, ?RE_WS_OPT) of
		{match, ["ws", Domain, Path]} ->
			Host=Domain,
			{normal, Domain, Host, ?DEFAULT_PORT, Path};
		{match, ["wss", Domain, Path]} ->
			Host=Domain,
			{secure, Domain, Host, ?DEFAULT_SECP, Path};
		nomatch ->
			parse_2(Url)
	end.
%------------------------------------------------------------------------------
% Url without Path
parse_2(Url) ->
	case re:run(Url, ?RE_WS_URL3, ?RE_WS_OPT) of
		{match, ["ws", Domain, Port]} ->
			Host = Domain++":"++Port,
			{normal, Domain, Host, list_to_integer(Port), ?DEFAULT_PATH};
		{match, ["wss", Domain, Port]} ->
			Host = Domain++":"++Port,
			{secure, Domain, Host, list_to_integer(Port), ?DEFAULT_PATH};
		nomatch ->
			parse_3(Url)
	end.
%------------------------------------------------------------------------------
% Url without Port and Path
parse_3(Url) ->
	case re:run(Url, ?RE_WS_URL4, ?RE_WS_OPT) of
		{match, ["ws", Domain]} ->
			Host=Domain,
			{normal, Domain, Host, ?DEFAULT_PORT, ?DEFAULT_PATH};
		{match, ["wss", Domain]} ->
			Host=Domain,
			{secure, Domain, Host, ?DEFAULT_SECP, ?DEFAULT_PATH};
		nomatch ->
			error
	end.
%------------------------------------------------------------------------------
to_string({Mode, _Domain, Host, _Port, Path}) ->
	Scheme = scheme(Mode),
	Scheme ++ "://" ++ Host ++ Path;
to_string(_) ->
	erlang:error(badarg).
%------------------------------------------------------------------------------
scheme(normal) -> "ws";
scheme(secure) -> "wss";
scheme(_) -> 
	erlang:error(badarg).
