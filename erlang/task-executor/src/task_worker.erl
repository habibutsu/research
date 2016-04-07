-module(task_worker).

-behaviour(gen_server).

-callback run(Task :: term(), Args :: term()) ->
    {ok, Result :: term()} |
    {retry, Result :: term()}.

-export([
    start_link/2
]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    msg_queue :: atom(),
    executor :: atom()
}).

%%====================================================================
%% API
%%====================================================================

start_link(MsgQueueName, ExecutorName) ->
    gen_server:start_link(?MODULE, [MsgQueueName, ExecutorName], []).

%%====================================================================
%% Internals
%%====================================================================

schedule_next() ->
    erlang:send(self(), main_loop).

retry_task(Task, MsgQueue) ->
    Retries =  maps:get(retries, Task),
    case Retries of
        0 -> ok;
        Retries ->
            msg_queue:push(
                maps:update(retries, Retries-1, Task),
                MsgQueue)
    end.

run_handlers(Task, []) ->
    {ok, Task};
run_handlers(Task, [Handler|Handlers]) ->
    #{args := Args} = Task,
    case Handler(Task, Args) of
        {retry, RetryTask} ->
            {retry, RetryTask};
        {ok, UpdTask} ->
            run_handlers(UpdTask, Handlers)
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([MsgQueueName, ExecutorName]) ->
    process_flag(trap_exit, true),
    State = #state{
        msg_queue = MsgQueueName,
        executor = ExecutorName
    },
    schedule_next(),
    {ok, State}.

handle_call(Message, From, State) ->
    error_logger:info_msg("<~p> call / ~p from ~p", [self(), Message, From]),
    {reply, ok, State}.

handle_cast(Message, State) ->
    error_logger:info_msg("<~p> cast / ~p", [self(), Message]),
    {noreply, State}.

handle_info(main_loop, State) ->
    MsgQueue = State#state.msg_queue,
    Executor = State#state.executor,
    {ok, Task} = msg_queue:pop(MsgQueue),
    {ok, Handlers} = task_executor:get_handlers(Executor),
    #{from := From, is_reply := IsReply} = Task,
    try
        run_handlers(Task, Handlers)
    of
        {ok, Result} ->
            ResultTask = maps:put(result, Result, Task),
            case IsReply of
                true -> From ! ResultTask;
                false -> ok
            end,
            ok;
        {retry, RetryTask} ->
            retry_task(RetryTask, MsgQueue)
    catch
        Error:Reason ->
            error_logger:error_msg("~p:~p", [Error,Reason]),
            retry_task(Task, MsgQueue)
    end,
    schedule_next(),
    {noreply, State};
handle_info(Message, State) ->
    error_logger:info_msg("~p ~s info / Message: ~p", [self(), ?MODULE, Message]),
    {noreply, State}.

terminate(Reason, State) ->
    error_logger:info_msg("~p ~s terminate / Reason: ~p", [self(), ?MODULE, Reason]),
    State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.