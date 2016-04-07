-module(task_worker_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).


start_link(MsgQueueName, CallbackModule) ->
    supervisor:start_link(?MODULE, [MsgQueueName, CallbackModule]).

init([MsgQueueName, CallbackModule]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5},

    NumWorkers = 2,
    ChildSpecs = lists:map(
        fun(Num) ->  #{
            id => {task_worker, Num},
            start => {task_worker, start_link, [MsgQueueName, CallbackModule]},
            restart => permanent,
            shutdown => 500,
            type => worker,
            modules => [task_worker]}
        end,
        lists:seq(1, NumWorkers)),
    {ok, {SupFlags, ChildSpecs}}.
