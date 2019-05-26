%%%-------------------------------------------------------------------
%%% @author vshev4enko
%%% @copyright (C) 2019
%%% @doc
%%%
%%% @end
%%% Created : 05. May 2019 09:03
%%%-------------------------------------------------------------------
-module(erlybot).
-author("vshev4enko").
-include("erlybot_int.hrl").


%% API
-export([
    start_bot/1,
    get_bot_config/1,
    subscribe/1,
    get_me/1,
    get_updates/2,
    send_message/2,
    start_pooling/1,
    stop_pooling/1]).

%% API
get_me(Name) ->
    gen_server:call(Name, {?GET_ME, #{}}).

get_updates(Name, Params) ->
    gen_server:call(Name, {?GET_UPDATES, Params}).

send_message(Name, #{<<"chat_id">> := _, <<"text">> := _}=Params) ->
    gen_server:call(Name, {?SEND_MESSAGE, Params}).

forward_message(Name, #{<<"chat_id">> := _, <<"from_chat_id">> := _, <<"message_id">> := _}=Params) ->
    gen_server:call(Name, {?FORWARD_MESSAGE, Params}).

start_pooling(Name) ->
    gen_server:call(Name, start_pooling).

stop_pooling(Name) ->
    gen_server:call(Name, stop_pooling).

start_bot(#{name := _, token := _}=Cfg) ->
    supervisor:start_child(erlybot_sup, [Cfg]).

subscribe(Name) ->
    gen_server:call(Name, subscribe).

get_bot_config(BotName) ->
    case get_env(bots) of
        {ok, [_|_]=Configs} ->
            Fun = fun(#{name := Name}=Obj) when Name =:= BotName -> throw({ok, Obj});
                     (_) -> undefined
                end,
            case catch lists:foreach(Fun, Configs) of
                {ok, #{}} = R -> R;
                Any -> Any

            end;
        Reason -> {error, Reason}
    end.

get_env(Key) ->
    application:get_env(?MODULE, Key).
