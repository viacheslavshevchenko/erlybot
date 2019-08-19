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


%% API
-export([start_pooling/1,
         start_pooling/2,
         start_pooling_and_subscribe/1,
         stop_pooling/1,
         get_env/1,
         get_url/3,
         get_me/1,
         get_updates/2,
         set_webhook/2,
         delete_webhook/1,
         get_webhook_info/1,
         send_message/2,
         forward_message/2,
         send_photo/2,
         get_chat/2,
         get_chat_administrators/2]).


start_pooling_and_subscribe(Bot) ->
    start_pooling(Bot, [self()]).

start_pooling(Bot) ->
    start_pooling(Bot, []).

start_pooling(Bot, Subscribers) ->
    {ok, Server} = get_env(server),
    case get_token(Bot) of
        {ok, Token} ->
            Args = #{name => Bot, token => Token, server => Server, opts => #{subscribers => Subscribers}},
            Start = {erlybot_pooling, start_link, [Args]},
            supervisor:start_child(erlybot_sup, #{id => Bot, start => Start, restart => transient});
        false ->
            {error, token_not_found}
    end.

stop_pooling(Bot) ->
    case supervisor:terminate_child(erlybot_sup, Bot) of
        ok -> 
            supervisor:delete_child(erlybot_sup, Bot);
        Error ->
            Error
    end.

%% Getting updates api
get_me(Bot) ->
    api_call(Bot, <<"getMe">>).

get_updates(Bot, #{} = Args) ->
    api_call(Bot, <<"getUpdates">>, {json, Args#{timeout => 0}}).

set_webhook(Bot, #{url := _} = Args) ->
    api_call(Bot, <<"setWebhook">>, {json, Args}).

delete_webhook(Bot) ->
    api_call(Bot, <<"deleteWebhook">>).

get_webhook_info(Bot) ->
    api_call(Bot, <<"getWebhookInfo">>).

%% Methods
send_message(Bot, #{chat_id := _, text := _} = Args) ->
    api_call(Bot, <<"sendMessage">>, {json, Args}).

forward_message(Bot, #{chat_id := _, from_chat_id := _, message_id := _} = Args) ->
    api_call(Bot, <<"forwardMessage">>, {json, Args}).

send_photo(Bot, #{chat_id := _, photo := _} = Args) ->
    api_call(Bot, <<"sendPhoto">>, {json, Args}).



get_chat(Bot, #{chat_id := _} = Args) ->
    api_call(Bot, <<"getChat">>, {json, Args}).

get_chat_administrators(Bot, #{chat_id := _} = Args) ->
    api_call(Bot, <<"getChatAdministrators">>, {json, Args}).

api_call(Bot, Method) ->
    api_call(Bot, Method, null).

api_call(Bot, Method, Payload) ->
    {ok, Server} = get_env(server),
    {ok, Token} = get_token(Bot),
    api_call(Server, Token, Method, Payload).

api_call(Server, Token, Method, Payload) ->
    Url = get_url(Server, Token, Method),
    case do_api_call(Url, Payload) of
        {ok, {{_, 200, "OK"}, _, Data}} -> 
            jsx:decode(Data, [return_maps]);
        {ok, {{_, Code, Reason}, _, Data}} ->
            error_logger:error_msg("Error ~p, reason ~p", [Code, Reason]),
            jsx:decode(Data, [return_maps])
    end.


do_api_call(Url, null) ->
    httpc:request(get, {Url, []}, [], [{body_format, binary}], ?MODULE);
do_api_call(Url, {json, Body}) ->
    httpc:request(post, {Url, [], "application/json", jsx:encode(Body)}, [], [{body_format, binary}], ?MODULE).

get_url(Server, Token, Method) ->
    Server ++ "/bot" ++ Token ++ "/" ++ binary_to_list(Method).

get_env(Key) ->
    case application:get_env(?MODULE, Key) of
        {ok, _} = Res -> Res;
        Any -> Any
    end.

get_token(Bot) ->
    case get_env(bots) of
        {ok, Props} ->
            case lists:keyfind(Bot, 1, Props) of
                {Bot, [{token, Token}]} ->
                    {ok, Token};
                false ->
                    false
            end;
        undefined ->
            false
    end.

