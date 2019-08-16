-module(erlybot_app).
-behaviour(application).


%% API
-export([start/2,
         stop/1]).


start(_Type, _Args) ->
	ssl:start(),
	inets:start(),
	inets:start(httpc, [{profile, erlybot}]),
	erlybot_sup:start_link().

stop(_State) ->
	ok.
