-module(task_executor).

-behaviour(supervisor).

-export([
    start_link/2,
    add_handler/2,
    get_handlers/1,
    new_task/2,
    new_task/3,
    wait_result/1,
    init/1
]).

%%====================================================================
%% API
%%====================================================================

start_link(ExecutorName, MsgQueueCapacity) ->
    supervisor:start_link(
        ?MODULE, [ExecutorName, MsgQueueCapacity]).

add_handler(Handler, ExecutorName) when is_function(Handler, 2) ->
    AExecutorName = erlang:binary_to_atom(ExecutorName, utf8),
    [{handlers, Handlers}] = ets:lookup(AExecutorName, handlers),
    UpdHandlers = lists:append([Handler], Handlers),
    ets:insert(AExecutorName, {handlers, UpdHandlers}),
    ok;
add_handler(ExecutorName, Handler) when is_function(Handler, 2)->
    AExecutorName = erlang:binary_to_atom(ExecutorName, utf8),
    [{handlers, Handlers}] = ets:lookup(AExecutorName, handlers),
    UpdHandlers = lists:append(Handlers, [Handler]),
    ets:insert(AExecutorName, {handlers, UpdHandlers}),
    ok;
add_handler(_, _) ->
    {error, invalid_handler, <<"Handler should be a function with arity 2">>}.

get_handlers(ExecutorName) when is_binary(ExecutorName) ->
    get_handlers(erlang:binary_to_atom(ExecutorName, utf8));
get_handlers(AExecutorName) ->
    [{handlers, Handlers}] = ets:lookup(AExecutorName, handlers),
    {ok, Handlers}.

new_task(ExecutorName, Args) ->
    new_task(ExecutorName, Args, #{}).

new_task(ExecutorName, Args, Options) ->
    MaxRetries = maps:get(max_retries, Options, 3),
    IsReply = maps:get(is_reply, Options, false),
    TaskId = uuid_v4(),
    Task = #{
        % The unique id of the executing task.
        id => TaskId,
        % How many times the current task has been retried.
        retries => MaxRetries,
        start_at => os:timestamp(),
        timeout => 10*60,
        args => Args,
        from => self(),
        is_reply => IsReply,
        result => undefined
    },
    MsgQueueName = erlang:binary_to_atom(
        <<ExecutorName/binary, "_queue">>, utf8),
    msg_queue:push(MsgQueueName, Task),
    {ok, TaskId}.


wait_result(TaskId) ->
    receive
        #{id := TaskId} = ResultTask ->
            ResultTask
    end.

%%====================================================================
%% Internals
%%====================================================================

%% Variant, corresponds to variant 1 0 of RFC 4122.
-define(VARIANT10, 2#10).

%% Version
-define(UUIDv4, 4).

uuid_v4() ->
    <<U0:32, U1:16, _:4, PU2:12, _:2, PU3:14, U4:48>> = crypto:rand_bytes(16),
    <<U2:16>> = <<?UUIDv4:4, PU2:12>>,
    <<U3:16>> = <<?VARIANT10:2, PU3:14>>,
    list_to_binary(
        io_lib:format(
            "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
            [U0, U1, U2, U3, U4])).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([ExecutorName, MsgQueueCapacity]) ->
    AExecutorName = erlang:binary_to_atom(ExecutorName, utf8),
    ets:new(
        AExecutorName, [
            named_table,
            public,
            {read_concurrency, true}
    ]),
    ets:insert(AExecutorName, {handlers, []}),

    SupFlags = #{
        strategy => one_for_all,
        intensity => 1,
        period => 5},
    MsgQueueName = erlang:binary_to_atom(
        <<ExecutorName/binary, "_queue">>, utf8),
    ChildSpecs = [
        #{
            id => msg_queue,
            start => {msg_queue, start_link, [MsgQueueName, MsgQueueCapacity]},
            restart => permanent,
            shutdown => 500,
            type => worker,
            modules => [msg_queue]
        },
        #{
            id => task_worker_sup,
            start => {
                task_worker_sup,
                start_link,
                [MsgQueueName, AExecutorName]},
            restart => permanent,
            shutdown => 500,
            type => supervisor,
            modules => [task_worker_sup]}
    ],
    {ok, {SupFlags, ChildSpecs}}.