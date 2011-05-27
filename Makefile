# Author: Emiliano Carlos de Moraes Firmino @ 05/2011
SHELL=/bin/sh
.SUFFIXES:
.SUFFIXES: .beam .erl .hrl
.PHONY: clean test run #target that has no output file

#Erlang Configuration
PADIR=-pa ebin
MODULELIST=[gen_ws, ws_header, hixie76_lib]

run: all_test
	erl $(PADIR)

all_test: all
	cd test; erl -make

all: clean
	erl -make

clean: 
	rm -fv ebin/*
	rm -fv erl_crash.dump

test: all_test
	erl -noshell $(PADIR) \
	-eval "eunit:test($(MODULELIST),[verbose])" \
	-s init stop

test_verbose: test
	erl -noshell $(PADIR) \
	-eval "ws_header_tests:parse_test()" \
	-eval "ws_header_tests:to_string_test()" \
	-eval "io:format(\"make_trial:~n~p\", [hixie76_lib:make_trial()])" \
	-eval "io:format(\"request:~n~p~n\", [hixie76_lib:gen_request(\"ws://echo.websocket.org/\",\"http://websocket.org\")])" \
	-s init stop
