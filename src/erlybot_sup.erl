-module(erlybot_sup).
-behaviour(supervisor).

-export([
    init/1,
    start_link/0]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    ChildSpec = #{
        'id'      => 'erlybot_srv',
        'start'   => {'erlybot_srv', 'start_link', []},
        'restart' => 'transient',
        'type'    => 'worker'},
	{ok, {{simple_one_for_one, 1, 5}, [ChildSpec]}}.
