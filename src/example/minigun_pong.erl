-module(minigun_pong).
-author("Byron Wang").
-behaviour(gen_server).

-export([start_link/0]).
-export([start/0, ping/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

start() ->

  ChildSpec = #{
    start =>    {minigun_pong, start_link, []},
    restart =>  permanent,
    shutdown => 5000,
    type =>     worker,
    modules =>  [minigun_pong]
  },

  RevolverOpts = #{
    min_alive_ratio          => 1.00,
    reconnect_delay          => 1000,
    max_message_queue_length => 2,
    connect_at_start         => true
  },

  Opts = #{
    name       => pong,
    pool_size  => 2,
    pool_limit => 4,
    child_spec => ChildSpec,
    revolver_opts => RevolverOpts
  },

  minigun_sup:start_link(Opts).

ping() ->
  spawn(fun() ->
    Pid = minigun:pid(pong),
    io:format("Send ping from ~p~n", [Pid]),
    gen_server:call(Pid, ping)
  end).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, #state{}}.

handle_call(ping, _From, State) ->
  timer:sleep(5000),
  io:format("Reply pong from ~p~n", [self()]),
  {reply, pong, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions