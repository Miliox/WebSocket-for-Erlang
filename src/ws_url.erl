%% LUDUS - Grupo de Desenvolvimento de Jogos
%% TCC1: Trabalho de Conclusao de Curso - 1
%% Aluno: Emiliano Carlos de Moraes Firmino
%% Orientador : Prof Jucimar Jr ( jucimar.jr@gmail.com )
%% Objetivo : Modulo especifico das operacoes sobre WsUrl
%% Criado: 05/25/11 22:40:13 (HPC)

%% @author Emiliano Carlos de Moraes Firmino <elmiliox@gmail.com>
%% @copyright Emiliano@2011

-module(ws_url).
-author("elmiliox@gmail.com").
-vsn(1).
%------------------------------------------------------------------------------
-import(re).
-export([parse/1, to_string/1]).
%------------------------------------------------------------------------------
-define(RE_WS_URL1, "(ws|wss)://([\\w\\d\.]+):([0-9]+)(/[^$]*)").
-define(RE_WS_URL2, "(ws|wss)://([\\w\\d\.]+)(/[^$]*)").
%------------------------------------------------------------------------------
-define(DEFAULT_PORT, 80).
-define(DEFAULT_SECP, 443).
%------------------------------------------------------------------------------
-define(OPT, [{capture, all_but_first, list}]).
%------------------------------------------------------------------------------
parse(Url) ->
	case re:run(Url, ?RE_WS_URL1, ?OPT) of
		{match, ["ws", Domain, Port, Path]} ->
			{normal, Domain, list_to_integer(Port), Path};
		{match, ["wss", Domain, Port, Path]} ->
			{secure, Domain, list_to_integer(Port), Path};
		nomatch ->
			parse_1(Url)
	end.
%------------------------------------------------------------------------------
parse_1(Url) ->
	case re:run(Url, ?RE_WS_URL2, ?OPT) of
		{match, ["ws", Domain, Path]} ->
			{normal, Domain, ?DEFAULT_PORT, Path};
		{match, ["wss", Domain, Path]} ->
			{secure, Domain, ?DEFAULT_SECP, Path};
		nomatch ->
			error
	end.
%------------------------------------------------------------------------------
to_string({normal, Domain, ?DEFAULT_PORT, Path}) ->
	"ws://" ++ Domain ++ Path;
to_string({normal, Domain, Port, Path}) ->
	"ws://" ++ Domain ++ ":" ++ integer_to_list(Port) ++ Path;
to_string({secure, Domain, ?DEFAULT_SECP, Path}) ->
	"wss://" ++ Domain ++ Path;
to_string({secure, Domain, Port, Path}) ->
	"ws://" ++ Domain ++ ":" ++ integer_to_list(Port) ++ Path;
to_string(_) ->
	erlang:error(badarg).
