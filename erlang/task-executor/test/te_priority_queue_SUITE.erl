-module(te_priority_queue_SUITE).

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
    advanced_test/1,
    waiting_and_exit_test/1
]).

-define(QUEUE_NAME, test_msq_queue).
-define(MOD, te_priority_queue).
-define(CAPACITY, 100).

all() ->
    [
        simple_test,
        advanced_test,
        capacity_test,
        waiting_test,
        waiting_and_exit_test
    ].

init_per_suite(Config) ->
    {ok, MsgQueue} = ?MOD:start(?QUEUE_NAME, ?CAPACITY),
    [
        {queue, MsgQueue}
        |Config
    ].

end_per_suite(Config) ->
    MsgQueue = ?config(queue, Config),
    exit(MsgQueue, normal),
    ok.

simple_test(Config) ->
    Msg = {msg, <<"Payload">>},
    MsgQueue = ?config(queue, Config),
    ?MOD:push(MsgQueue, Msg, 1),
    {ok, Value} = ?MOD:length(MsgQueue),
    ?assertEqual(1, Value),
    ?assertMatch({ok, Msg},  ?MOD:pop(MsgQueue)),

    ?MOD:push(?QUEUE_NAME, Msg),
    ?assertMatch({ok, Msg}, ?MOD:pop(MsgQueue)),
    ok.

advanced_test(Config) ->
    Q = ?config(queue, Config),
    
    PriorityCount = 10,
    ItemsPerPriority = 5,
    N = PriorityCount * ItemsPerPriority,
    Priorities = lists:flatten([lists:duplicate(ItemsPerPriority, P) || P <- lists:seq(1, PriorityCount)]),
    Items = lists:seq(1, N),
    IP = lists:zip(Items, Priorities),

    % N times fill and empty the queue while checking the correct order of popped elements
    lists:foreach(
        fun(_) ->
            IP_Shuffled = random_shuffle(IP),
            lists:foreach(fun({I, P}) -> ?MOD:push(Q, I, P) end, IP_Shuffled),
            {_, Popped} = lists:unzip([?MOD:pop(Q) || _ <- lists:seq(1, N)]), % pop returns {ok, Value}           
            {Ex, _} = lists:unzip(lists:keysort(2, lists:reverse(IP_Shuffled))),
            Expected = lists:reverse(Ex),
            ?assertEqual(Expected, Popped)
        end,
        lists:seq(1, N)),

    {ok, L} = ?MOD:length(Q),
    ?assertEqual(0, L),
    ok.

random_shuffle(L) ->
    random:seed(now()),
    {LShuffled, _} = lists:foldl(
        fun(I, {Acc, Rest}) ->
            E = lists:nth(random:uniform(length(Rest)), Rest),
            {[E | Acc], lists:delete(E, Rest)}
        end,
        {[], L},
        L),
    LShuffled.

capacity_test(Config) ->
    MsgQueue = ?config(queue, Config),
    lists:foreach(
        fun(N) -> ?MOD:push(MsgQueue, N, N) end,
        lists:seq(1, ?CAPACITY)),
    Result = ?MOD:push(MsgQueue, ?CAPACITY + 1),

    ?assertMatch({error, capacity_exhausted},  Result),
    ?MOD:purge(MsgQueue),
    ok.


waiting_test(Config) ->
    MsgQueue = ?config(queue, Config),
    Pid = spawn_link(
        fun() ->
            ?MOD:pop(MsgQueue)
        end),
    timer:sleep(500),
    ?assert(erlang:is_process_alive(Pid)),
    ?MOD:push(MsgQueue, <<"message">>),
    timer:sleep(500),
    ?assertNot(erlang:is_process_alive(Pid)),
    ok.

waiting_and_exit_test(Config) ->
    MsgQueue = ?config(queue, Config),
    Pid = spawn_link(
        fun() ->
            ?MOD:pop(MsgQueue)
        end),
    timer:sleep(500),
    ?assert(erlang:is_process_alive(Pid)),
    exit(Pid, normal),
    ok.