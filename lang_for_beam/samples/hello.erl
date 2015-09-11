-module(hello).

-export([hello_world/0]).

% erlc hello.erl
% erl -noshell -s hello hello_world -s init stop

hello_world() ->
    %Args = init:get_arguments(),
    Args = init:get_plain_arguments(),
    io:fwrite("hello, world\n"),
    io:format("Init arguments: ~p~n", [Args]).