-module(erlybot_sup).
-behaviour(supervisor).

-export([init/1,
         start_link/0]).


start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Flags = #{strategy => one_for_one,
              intensity => 1,
              period => 5},
	{ok, {Flags, []}}.

