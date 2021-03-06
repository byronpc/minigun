-module(minigun).
-author("Byron Wang").
-include("minigun.hrl").

-export([
  pid/1,
  worker_count/1,
  add_worker/1,
  cleanup_workers/1
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Retrieves a pid from the pool
pid(PoolId) ->
  case catch revolver:pid(PoolId) of
    Pid when is_pid(Pid) ->
      Pid;
    {error, overload} ->
      add_worker(PoolId);
    _ ->
      {error, no_pool}
  end.

%% @doc Retrieves the number of workers in the pool
worker_count(PoolId) ->
  [_,_,_,{workers,Workers}] =
    supervisor:count_children(?SUPERVISOR(PoolId)),
  Workers.

%% @doc Dynamically adds a worker to the pool
add_worker(PoolId) ->
  WorkerCount = worker_count(PoolId),
  case application:get_env(PoolId, pool_limit) of
    {ok, PoolLimit} when PoolLimit > WorkerCount ->
      {ok, Pid} = supervisor:start_child(?SUPERVISOR(PoolId), get_transient_childspec(PoolId)),
      revolver:connect(PoolId),
      Pid;
    _ ->
      {error, overload}
  end.

%% @doc Cleans up idle workers from the pool
cleanup_workers(PoolId) ->
  Workers = supervisor:which_children(?SUPERVISOR(PoolId)),
  cleanup_workers(PoolId, Workers).

cleanup_workers(_PoolId, []) ->
  ok;

cleanup_workers(PoolId, [{PidName, Pid, _, _}|Rest]) ->
  case supervisor:get_childspec(?SUPERVISOR(PoolId), PidName) of
    % If permanent worker, skip cleanup
    {ok, #{restart := permanent}} ->
      skip;
    _ ->
      case revolver_utils:message_queue_len(Pid) of
        0 -> decommision_worker(PoolId, PidName, Pid);
        _ -> skip
      end
  end,
  cleanup_workers(PoolId, Rest).

%%====================================================================
%% Private functions
%%====================================================================

%% Remove worker from the pool
decommision_worker(PoolId, PidName, Pid) ->
  % Remove pid from pool
  ok = revolver:pid_down(PoolId, Pid),
  % Stop the pid
  ok = gen_server:stop(Pid),
  % Delete remove child from supervisor
  ok = supervisor:delete_child(?SUPERVISOR(PoolId), PidName).

%% Dynamically added workers to the pool will have transient childspec
get_transient_childspec(PoolId) ->
  case application:get_env(PoolId, child_spec) of
    {ok, {_Id, {M, F, A}, _Restart, Shutdown, Type, Modules}} ->
      #{
        id => ?GENERATE_WORKER_ID(PoolId),
        modules => Modules,
        restart => transient,
        shutdown => Shutdown,
        start => {M,F,A},
        type => Type
      };
    {ok, Template} ->
      maps:merge(Template, #{
        id => ?GENERATE_WORKER_ID(PoolId),
        restart => transient
      })
  end.
