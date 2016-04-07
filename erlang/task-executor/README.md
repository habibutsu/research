# Overview

Implementing message queue and executor of task

# Usage

### Example of using "task_executor"

```
ExecutorName = <<"my_task_executor">>,
QueueCapacity = infinity,
{ok, TaskExecutor} = task_executor:start_link(ExecutorName, QueueCapacity),

task_executor:add_handler(
    <<"my_task_executor">>,
    fun(Task, [A, B]) ->
        {ok, A + B}
    end),
{ok, TaskId} = task_executor:new_task(
    <<"my_task_executor">>, [1, 2], #{is_reply=>true}),
Task = task_executor:wait_result(TaskId),
#{result := Result} = Task.
```

Result:

```
#{args => [1,2],
  from => <0.39.0>,
  id => <<"e930f3fa-f6ce-40a3-baab-0ca4913f3455">>,
  is_reply => true,
  result => 3,
  retries => 3,
  start_at => {1438,893690,33475},
  timeout => 600}
```

### Example of using "msg_queue"

```
QueueName = my_msg_queue,
Capacity = 10,
{ok, MsgQueue} = msg_queue:start(QueueName, Capacity),
lists:foreach(
    fun(N) -> msg_queue:push({msg, N}, QueueName) end,
    lists:seq(1, 100)).
{ok, 10} = msg_queue:length(QueueName).
```
