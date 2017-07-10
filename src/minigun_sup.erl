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

start_link(#{name := Name,
             pool_size := Size,
             child_spec := ChildSpec,
             revolver_opts := RevolverOpts} = Opts) ->

  % Save Opts in Application env
  maps:map(fun(K,V) -> application:set_env(Name, K, V) end, Opts),

  % Start supervisor
  Supervisor = supervisor:start_link({local, ?SUPERVISOR(Name)}, ?MODULE, [Name, Size, ChildSpec]),

  % Automatically balance in revolver
  revolver:balance(?SUPERVISOR(Name), Name, RevolverOpts),

  Supervisor.

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([Name, Size, ChildSpec]) ->
  ChildSpecs = generate_childspecs(Name, Size, ChildSpec),
  SupFlags = #{
    strategy => one_for_one,
    intensity => 1,
    period => 5},
  {ok, {SupFlags, ChildSpecs}}.

generate_childspecs(Name, Size, ChildSpec) ->
  Template = get_childspec(ChildSpec),
  lists:map(fun(X) ->
    maps:put(id, ?WORKER(Name, X), Template)
  end, lists:seq(1, Size)).

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
