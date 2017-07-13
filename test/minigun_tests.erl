-module(minigun_tests).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("minigun.hrl").

-define(POOL_ID, pong).
-define(POOL_SIZE, 2).
-define(POOL_LIMIT, 3).
-define(JANITOR_TTL, 5000).

minigun_test_() ->
  {setup, fun start/0, fun stop/1, []}.

start() ->
  minigun_sup:start_link(#{
    pool_id       => ?POOL_ID,
    pool_size     => ?POOL_SIZE,
    pool_limit    => ?POOL_LIMIT,
    janitor_ttl   => ?JANITOR_TTL,
    child_spec    => #{
      start     => {minigun_pong, start_link, []},
      restart   => permanent,
      shutdown  => 5000,
      type      => worker,
      modules   => [minigun_pong]
    },
    revolver_opts => #{
      min_alive_ratio          => 1.00,
      reconnect_delay          => 1000,
      max_message_queue_length => 2,
      connect_at_start         => true
    }
  }).

stop(_Pid) ->
  ok.

worker_count_test() ->
  minigun:cleanup_workers(?POOL_ID),
  Size = minigun:worker_count(?POOL_ID),
  ?assertEqual(?POOL_SIZE, Size).

add_worker_test() ->
  minigun:cleanup_workers(?POOL_ID),
  Size = minigun:worker_count(?POOL_ID),
  minigun:add_worker(?POOL_ID),
  NewSize = minigun:worker_count(?POOL_ID),
  ?assertEqual(NewSize, Size + 1).

cleanup_worker_test() ->
  minigun:add_worker(?POOL_ID),
  minigun:cleanup_workers(?POOL_ID),
  Size = minigun:worker_count(?POOL_ID),
  ?assertEqual(?POOL_SIZE, Size).

pid_test() ->
  Pids = revolver_utils:child_pids(?SUPERVISOR(?POOL_ID)),
  Size = minigun:worker_count(?POOL_ID),
  Pids2 = lists:map(fun(_) ->
    minigun:pid(?POOL_ID)
  end, lists:seq(1, Size)),
  ?assertEqual(lists:usort(Pids), lists:usort(Pids2)).

get_pid_response_test_() ->
    % sleep for 10 seconds
    {timeout, 10, ?_assertEqual(pong, gen_server:call(minigun:pid(?POOL_ID), ping, 15000))}.

max_dynamic_pool_test() ->
  F = fun() ->
    spawn(fun() -> gen_server:call(minigun:pid(?POOL_ID), ping, 15000) end),
    timer:sleep(10),
    spawn(fun() -> gen_server:call(minigun:pid(?POOL_ID), ping, 15000) end),
    timer:sleep(10)
  end,
  minigun:cleanup_workers(?POOL_ID),
  F(), % 0
  F(), % 1
  F(), % 2
  Size = minigun:worker_count(?POOL_ID),
  ?assertEqual(?POOL_SIZE, Size),
  F(), % 3
  F(),
  NewSize = minigun:worker_count(?POOL_ID),
  ?assertEqual(?POOL_SIZE + 1, NewSize),
  F(),
  ?assertEqual({error, overload}, minigun:pid(?POOL_ID)).

-endif.
