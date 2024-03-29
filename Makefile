# Author: Emiliano Carlos de Moraes Firmino @ 05/2011
SHELL=/bin/sh
.SUFFIXES:
.SUFFIXES: .beam .erl .hrl
.PHONY: clean test run #target that has no output file

#Erlang Configuration
PADIR=-pa ebin
MODULELIST=[gen_ws, wslib.header, wslib.url, wslib.hixie76, wslib.hybi07]

run: all_test
	erl $(PADIR)

all_test: all
	cd test; erl -make

all: clean
	erl -make

clean: 
	rm -fv ebin/*.beam
	rm -fv ebin/wslib/*.beam
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
	-eval "{Q,_} = hixie76_lib:gen_request(\"ws://echo.websocket.org/test\",\"http://websocket.org\"), R = hixie76_lib:gen_response(Q), io:format(\"~nrequest:~n~p~nresponse:~n~p~n\", [Q,R])" \
	-s init stop
