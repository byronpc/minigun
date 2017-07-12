%%%-------------------------------------------------------------------
%% @doc minigun top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(minigun_sup).
-author("Byron Wang").
-behaviour(supervisor).
-include("minigun.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(#{pool_id := PoolId,
             pool_size := PoolSize,
             janitor_ttl := JanitorTtl,
             child_spec := ChildSpec,
             revolver_opts := RevolverOpts} = Opts) ->

  % Save Opts in Application env
  maps:map(fun(K,V) -> application:set_env(PoolId, K, V) end, Opts),

  % Start supervisor
  Supervisor = supervisor:start_link({local, ?SUPERVISOR(PoolId)}, ?MODULE, [PoolId, PoolSize, ChildSpec]),

  % Automatically balance in revolver
  revolver:balance(?SUPERVISOR(PoolId), PoolId, RevolverOpts),

  % Start janitor (if applicable)
  start_janitor(PoolId, JanitorTtl),

  Supervisor.

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([PoolId, PoolSize, ChildSpec]) ->
  ChildSpecs = generate_childspecs(PoolId, PoolSize, ChildSpec),
  SupFlags = #{
    strategy => one_for_one,
    intensity => 1,
    period => 5},
  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Private functions
%%====================================================================

start_janitor(_PoolId, 0) -> disabled;

start_janitor(PoolId, Ttl) ->
  timer:apply_interval(Ttl, minigun, cleanup_workers, [PoolId]).

generate_childspecs(PoolId, PoolSize, ChildSpec) ->
  Template = get_childspec(ChildSpec),
  lists:map(fun(_X) ->
    maps:put(id, ?GENERATE_WORKER_ID(PoolId), Template)
  end, lists:seq(1, PoolSize)).

get_childspec({Id, Mfa, Restart, Shutdown, Type, Modules}) ->
  #{
    id => Id,
    start => Mfa,
    restart => Restart,
    shutdown => Shutdown,
    type => Type,
    modules => Modules
  };

get_childspec(Map) -> Map.
