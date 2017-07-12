-define(SUPERVISOR(PoolId), list_to_atom(atom_to_list(PoolId) ++ "_sup")).
-define(GENERATE_WORKER_ID(PoolId), list_to_atom(
  atom_to_list(PoolId) ++ "_" ++ integer_to_list(erlang:system_time(microsecond)))).