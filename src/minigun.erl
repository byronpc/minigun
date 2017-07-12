-module(minigun).
-author("Byron Wang").
-include("minigun.hrl").

-export([
  pid/1,
  worker_count/1,
  add_worker/1,
  cleanup_workers/1,
  test/0
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Retrieves a pid from the pool
pid(Pool) ->
  case catch revolver:pid(Pool) of
    Pid when is_pid(Pid) ->
      Pid;
    {error, overload} ->
      add_worker(Pool);
    _ ->
      {error, no_pool}
  end.

%% @doc Retrieves the number of workers in the pool
worker_count(Pool) ->
  [_,_,_,{workers,Workers}] =
    supervisor:count_children(?SUPERVISOR(Pool)),
  Workers.

%% @doc Dynamically adds a worker to the pool
add_worker(Pool) ->
  WorkerCount = worker_count(Pool),
  case application:get_env(Pool, pool_limit) of
    {ok, PoolOverflow} when PoolOverflow > WorkerCount ->
      {ok, Pid} = supervisor:start_child(?SUPERVISOR(Pool), get_transient_childspec(Pool)),
      revolver:connect(Pool),
      Pid;
    _ ->
      {error, overload}
  end.

%% @doc Cleans up idle workers from the pool
cleanup_workers(Pool) ->
  Workers = supervisor:which_children(?SUPERVISOR(Pool)),
  cleanup_workers(Pool, Workers).

cleanup_workers(_Pool, []) ->
  ok;

cleanup_workers(Pool, [{PidName, Pid, _, _}|Rest]) ->
  case supervisor:get_childspec(?SUPERVISOR(Pool), PidName) of
    % If permanent worker, skip cleanup
    {ok, #{restart := permanent}} ->
      skip;
    _ ->
      case revolver_utils:message_queue_len(Pid) of
        0 ->
          decommision_worker(Pool, PidName, Pid);
        _ ->
          skip
      end
  end,
  cleanup_workers(Pool, Rest).

%%====================================================================
%% Private functions
%%====================================================================

%% Remove worker from the pool
decommision_worker(Pool, PidName, Pid) ->
  % MAKE SURE NOT ACCEPTING NEW MESSAGES REMOVE FROM REVOLVER
  ok = gen_server:stop(Pid),
  ok = supervisor:delete_child(?SUPERVISOR(Pool), PidName).

%% Dynamically added workers to the pool will have trancient childspec
get_transient_childspec(Pool) ->
  {ok, Template} = application:get_env(Pool, child_spec),
  maps:merge(Template, #{
    id => ?WORKER(Pool, worker_count(Pool) + 1),
    restart => transient
  }).

test() ->
  minigun_pong:start(),
  P = minigun:add_worker(pong),
  spawn(fun() -> gen_server:call(P, ping, 10000) end).

