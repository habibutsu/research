-module(task_handler).

-behaviour(task_worker).

-export([
    init/1,
    process_task/3
]).

init([]) ->
    {ok, []}.

process_task(_TaskId, _Args, State) ->
    {ok, State}.
