-module(te_priority_queue).

-behaviour(gen_server).


% API
-export([
    start/1,
    start_link/1,
    start/2,
    start_link/2,
    push/2,
    push/3,
    pop/1,
    async_pop/1,
    purge/1,
    length/1
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
    queue_map = maps:new() :: map(),
    queue_length = 0 :: non_neg_integer(),
    queue_capacity = infinity,
    highest_priority = neg_infinity,
    waiting_calls = [] :: [pid()],
    monitors = maps:new() :: map()
}).

%%====================================================================
%% API
%%====================================================================


start(Name) ->
    start(Name, infinity).

start(Name, Capacity) ->
    gen_server:start({local, Name}, ?MODULE, [Capacity], []).

start_link(Name) ->
    start_link(Name, infinity).

start_link(Name, Capacity) ->
    gen_server:start_link({local, Name}, ?MODULE, [Capacity], []).

push(MsgQueue, Msg) ->
    push(MsgQueue, Msg, 0).

push(MsgQueue, Msg, Priority) ->
    gen_server:call(MsgQueue, {push, Msg, Priority}).

pop(MsgQueue) ->
    gen_server:call(MsgQueue, {pop}, infinity).

async_pop(MsgQueue) ->
    gen_server:cast(MsgQueue, {pop, self()}).

purge(MsgQueue) ->
    gen_server:call(MsgQueue, {purge}).

length(MsgQueue) ->
    gen_server:call(MsgQueue, {length}).

%%====================================================================
%% Internals
%%====================================================================

put_waiting_call({Pid, _Tag} = From, State) ->
    WaitingCalls = State#state.waiting_calls,
    Monitors = maps:put(
        From,
        erlang:monitor(process, Pid),
        State#state.monitors),
    State#state{
        waiting_calls=[From | WaitingCalls],
        monitors=Monitors
    }.

pop_waiting_call(State) ->
    [From|WaitingCalls] = State#state.waiting_calls,
    MonitorRef = maps:get(From, State#state.monitors),
    erlang:demonitor(MonitorRef),
    Monitors = maps:remove(From, State#state.monitors),
    {From, State#state{
        waiting_calls=WaitingCalls,
        monitors=Monitors}}.

remove_waiting_call(Pid, State) ->
    WaitingCalls = State#state.waiting_calls,
    Monitors = maps:remove(Pid, State#state.monitors),
    State#state{
        waiting_calls=WaitingCalls--[Pid],
        monitors=Monitors}.

enque(Task, Priority, State) ->
    M = State#state.queue_map,
    QNew = case maps:get(Priority, State#state.queue_map, undefined) of
        undefined -> 
            Q1 = queue:new(),
            queue:in(Task, Q1);
        Q ->
            queue:in(Task, Q)
    end,

    HPNew = case State#state.highest_priority of
        neg_infinity -> Priority;
        HP -> max(HP, Priority)
    end,

    State#state{
        queue_length = State#state.queue_length + 1,
        queue_map = maps:put(Priority, QNew, M),
        highest_priority = HPNew}.

dequeue(State) ->
    case State#state.highest_priority of
        neg_infinity ->
            {empty, State};
        HP ->
            M = State#state.queue_map,
            Q = maps:get(HP, M),

            case queue:out(Q) of
                {{value, Task}, QN} ->
                    {Task,
                     State#state{
                        queue_map = maps:update(HP, QN, M),
                        queue_length = State#state.queue_length - 1}};
                {empty, _} ->
                    % find highest priority of all non-empty queues
                    case maps:fold(fun get_highest_priority/3, neg_infinity, M) of
                        neg_infinity ->
                            {empty, State#state{highest_priority = neg_infinity}};
                        NextHighestPriority ->
                            dequeue(State#state{highest_priority = NextHighestPriority})
                    end
            end        
    end.

get_highest_priority(Key, Value, Acc) ->
    case queue:is_empty(Value) of
        true -> Acc;
        false ->
            case Acc of
                neg_infinity -> Key;
                _ -> max(Acc, Key)
            end
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Capacity]) ->
    process_flag(trap_exit, true),
    State = #state{
        queue_capacity = Capacity
    },
    {ok, State}.

handle_call({push, Msg, Priority}, _From, State) ->
    QCapacity = State#state.queue_capacity,
    QLength = State#state.queue_length,
    IsOverCapacity = case QCapacity of
        infinity ->
            false;
        _ ->
            QLength >= QCapacity
    end,
    case IsOverCapacity of
        true ->
            {reply, {error, capacity_exhausted}, State};
        false ->
            case State#state.waiting_calls of
                [] ->
                    {reply, ok, enque(Msg, Priority, State)};
                _ ->
                    {Client, NewState} = pop_waiting_call(State),
                    case Client of
                        {Pid, no_tag} ->
                            Pid ! {task, Msg};
                        _ ->
                            gen_server:reply(Client, {ok, Msg})
                    end,
                    {reply, ok, NewState}
            end
    end;
handle_call({pop}, From, State) ->
    case dequeue(State) of
        {empty, _NewState} ->
            {noreply, put_waiting_call(From, State)};
        {Msg, NewState} ->
            {reply, {ok, Msg}, NewState}
    end;
handle_call({purge}, _From, _State) ->
    UpdState = #state{
        queue_map = maps:new(),
        queue_length = 0},
    {reply, ok, UpdState};
handle_call({length}, _From, State) ->
    {reply, {ok, State#state.queue_length}, State};
handle_call(Message, From, State) ->
    error_logger:info_msg("<~p> call / ~p from ~p", [self(), Message, From]),
    {reply, ok, State}.

handle_cast({pop, Pid}, State) ->
    From = {Pid, no_tag},
    case dequeue(State) of
        {empty, _} ->
            {noreply, put_waiting_call(From, State)};
        {Task, NewState} ->
            Pid ! {task, Task},
            {noreply, NewState}
    end;
handle_cast(Message, State) ->
    error_logger:info_msg("<~p> cast / ~p", [self(), Message]),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, State) ->
    {noreply, remove_waiting_call(Pid, State)};
handle_info(Message, State) ->
    error_logger:info_msg("~p ~s info / Message: ~p", [self(), ?MODULE, Message]),
    {noreply, State}.


terminate(Reason, State) ->
    error_logger:info_msg("~p ~s terminate / Reason: ~p", [self(), ?MODULE, Reason]),
    State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
