-module(msg_queue_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    simple_test/1,
    capacity_test/1,
    waiting_test/1,
    waiting_and_exit_test/1
]).

-define(QUEUE_NAME, test_msq_queue).

all() ->
    [
        simple_test,
        capacity_test,
        waiting_test,
        waiting_and_exit_test
    ].

init_per_suite(Config) ->
    {ok, MsgQueue} = msg_queue:start(?QUEUE_NAME, 10),
    [
        {msg_queue, MsgQueue}
        |Config
    ].

end_per_suite(Config) ->
    MsgQueue = ?config(msg_queue, Config),
    exit(MsgQueue, normal),
    ok.

simple_test(Config) ->
    Msg = {msg, <<"Payload">>},
    MsgQueue = ?config(msg_queue, Config),
    msg_queue:push(MsgQueue, Msg),
    {ok, Value} = msg_queue:length(MsgQueue),
    ?assertEqual(1, Value),
    ?assertMatch({ok, Msg},  msg_queue:pop(MsgQueue)),

    msg_queue:push(?QUEUE_NAME, Msg),
    ?assertMatch({ok, Msg}, msg_queue:pop(MsgQueue)),
    ok.

capacity_test(Config) ->
    MsgQueue = ?config(msg_queue, Config),
    lists:foreach(
        fun(N) -> msg_queue:push(MsgQueue, N) end,
        lists:seq(1, 10)),
    Result = msg_queue:push(MsgQueue, 11),

    ?assertMatch({error, capacity_exhausted},  Result),
    msg_queue:purge(MsgQueue),
    ok.


waiting_test(Config) ->
    MsgQueue = ?config(msg_queue, Config),
    Pid = spawn_link(
        fun() ->
            msg_queue:pop(MsgQueue)
        end),
    timer:sleep(500),
    ?assert(erlang:is_process_alive(Pid)),
    msg_queue:push(MsgQueue, <<"message">>),
    timer:sleep(500),
    ?assertNot(erlang:is_process_alive(Pid)),
    ok.

waiting_and_exit_test(Config) ->
    MsgQueue = ?config(msg_queue, Config),
    Pid = spawn_link(
        fun() ->
            msg_queue:pop(MsgQueue)
        end),
    timer:sleep(500),
    ?assert(erlang:is_process_alive(Pid)),
    exit(Pid, normal),
    ok.