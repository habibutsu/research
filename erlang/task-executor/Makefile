all:
	rebar compile

test: all
	rebar skip_deps=true ct

run:
	erl -name repl@127.0.0.1 -pa ./ebin

attach:
	erl -name repl-remote@127.0.0.1 -remsh repl@127.0.0.1
