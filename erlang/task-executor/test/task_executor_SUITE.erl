-module(task_executor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    simple_test/1
]).

all() ->
    [
        simple_test
    ].

init_per_suite(Config) ->
    {ok, TaskExecutor} = task_executor:start_link(
        <<"my_executor">>, infinity),
    erlang:unlink(TaskExecutor),
    [
        {task_executor, TaskExecutor}
        |Config
    ].

end_per_suite(Config) ->
    TaskExecutor = ?config(task_executor, Config),
    exit(TaskExecutor, normal),
    ok.

simple_test(_Config) ->
    Pid = self(),
    task_executor:add_handler(
        <<"my_executor">>,
        fun(Task, [A, B]) ->
            {ok, A + B}
        end),
    {ok, TaskId} = task_executor:new_task(
        <<"my_executor">>, [1, 2], #{is_reply=>true}),
    ResultTask = task_executor:wait_result(TaskId),
    #{result := Result} = ResultTask,
    ?assertEqual(3, Result),
    ok.
