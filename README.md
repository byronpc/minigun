# minigun

minigun is a round-robin load balancer built on top of [revolver](https://github.com/odo/revolver) with additional features such as dynamic pooling

## configuration

### revolver options (See [revolver](https://github.com/odo/revolver) for more information)
```erlang
RevolverOpts = #{
  min_alive_ratio          => 1.00,
  reconnect_delay          => 1000,
  max_message_queue_length => 2,
  connect_at_start         => true
}
```

### worker child spec
```erlang
ChildSpec = #{
    start =>    {minigun_pong, start_link, []},
    restart =>  permanent,
    shutdown => 5000,
    type =>     worker,
    modules =>  [minigun_pong]
}
```

### Minigun Opts
```erlang
MinigunOpts = #{
    name          => pong,        % poolname
    pool_size     => 2,         % initial size of the pool
    pool_limit    => 4,         % maximum size of the pool
    child_spec    => ChildSpec,   % worker child spec
    revolver_opts   => RevolverOpts % revolver options
}
```

## usage

You can start multiple minigun instances.

To manually start the supervisor:
```erlang
1> minigun_sup:start_link(Opts)
{ok,<0.122.0>}
```

To include minigun in your supervisor:
```erlang
SupervisorOpts = #{
  id =>     minigun_sup
    start =>    {minigun_sup, start_link, [MinigunOpts]},
    restart =>  permanent,
    shutdown => infinity,
    type =>     supervisor,
    modules =>  [minigun_sup]
}
```

Elixir:
```elixir
opts = %{
  name: pong,
    pool_size:  2,
    pool_limit: 4,
    child_spec: worker(:minigun_pong, []),
    revolver_opts: %{
      min_alive_ratio: 1.00,
      reconnect_delay: 1000,
      max_message_queue_length: 2,
      connect_at_start: true
    }
}

supervisor(:minigun_sup, [opts])
```