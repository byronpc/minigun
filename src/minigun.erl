-module(minigun).
-author("Byron Wang").
-include("minigun.hrl").

-export([
  pid/1,
  worker_count/1,
  add_worker/1
]).

pid(Name) ->
  case catch revolver:pid(Name) of
    Pid when is_pid(Pid) ->
      Pid;
    {error, overload} ->
      add_worker(Name);
    _ ->
      {error, no_pool}
  end.

worker_count(Name) ->
  [_,_,_,{workers,Workers}] =
    supervisor:count_children(?SUPERVISOR(Name)),
  Workers.

add_worker(Name) ->
  WorkerCount = worker_count(Name),
  case application:get_env(Name, pool_limit) of
    {ok, PoolOverflow} when PoolOverflow > WorkerCount ->
      {ok, Template} = application:get_env(Name, child_spec),
      ChildSpec = maps:put(id, ?WORKER(Name, worker_count(Name) + 1), Template),
      {ok, Pid} = supervisor:start_child(?SUPERVISOR(Name), ChildSpec),
      revolver:connect(Name),
      Pid;
    _ ->
      {error, overload}
  end.
