-module(erlybot_srv).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-include("erlybot_int.hrl").

%% gen_server.
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% Base url
-define(BOT_URL, "https://api.telegram.org/bot").


-record(state, {
    token       = null,
    offset      = 0,
    info        = #{},
    update_ref  = null,
    subscribers = []
}).

-define(DEFAULT_TIMEOUT, 100).


%% API.
start_link(#{name := Name, token := Token}) ->
	gen_server:start_link({local, Name}, ?MODULE, [Token], []).

init([Token]) ->
%%    ok = trigger_task(start_pooling),
	{ok, #state{token = Token}}.

handle_call(subscribe, {Pid, _}, #state{subscribers = Subscribers} = State) ->
    NewSubscribers = lists:usort([Pid|Subscribers]),
    {reply, {ok, NewSubscribers}, State#state{subscribers = NewSubscribers}};
handle_call(start_pooling, _From, State) ->
    Url = get_url(State#state.token, ?GET_UPDATES),
    Body = jsx:encode(#{
        <<"offset">> => State#state.offset,
        <<"timeout">> => ?DEFAULT_TIMEOUT
    }),
    {Resp, NewState} = case send_request(Url, Body) of
        {ok, Ref} -> {ok, State#state{update_ref = Ref}};
        Reason    -> {{error, Reason}, State}
    end,
    {reply, Resp, NewState};
handle_call(stop_pooling, _From, State) ->
    {reply, ok,  State#state{update_ref = null}};
handle_call({Method, Body}, _From, State) ->
    Url = get_url(State#state.token, Method),
    JsonBody = jsx:encode(Body),
    Resp = case send_request(Url, JsonBody) of
               {ok, _} -> ok;
               Error   -> Error
    end,
    {reply, Resp, State};
handle_call(Msg, _From, State) ->
    error_logger:info_msg("~Msg ~p", [Msg]),
    {reply, ok, State}.
handle_cast(Msg, State) ->
    error_logger:info_msg("~Msg ~p", [Msg]),
    {noreply, State}.
handle_info({http, {Ref, stream, Json}}, State) when Ref =:= State#state.update_ref ->
    MSG = jsx:decode(Json, [return_maps]),
    ok = trigger_task(start_pooling),
    {noreply, handle_response(MSG, State)};
handle_info({http, {_, stream, Json}}, State) ->
    MSG = jsx:decode(Json, [return_maps]),
    {noreply, handle_response(MSG, State)};
handle_info(_Msg, State) ->
%%    error_logger:info_msg("~nMSG ~p", [_Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

send_request(Url, Body) ->
    Options = [
        {sync, false},
        {full_result, true},
        {stream, self},
        {body_format, binary}],
    case httpc:request(post, {Url, [], "application/json", Body}, [], Options) of
        {ok, Ref} ->
            {ok, Ref};
        {error, Reason} ->
            error_logger:info_msg("~nError ~p", [Reason]),
            {error, Reason}
    end.

get_url(Token, Method) ->
    ?BOT_URL ++ Token ++ Method.

handle_response(#{<<"ok">> := true, <<"result">> := #{} = Result}, State) ->
    ok = notify_subscribers([Result], State#state.subscribers),
    State;
handle_response(#{<<"ok">> := true, <<"result">> := [_|_] = ListOfMsgs}, State) ->
    F = fun(#{<<"update_id">> := UpdId}, Acc) when UpdId > Acc -> UpdId end,
    LastMsgId = lists:foldl(F,  0, ListOfMsgs),
    ok = notify_subscribers(ListOfMsgs, State#state.subscribers),
    State#state{offset = LastMsgId+1};
handle_response(#{<<"ok">> := true, <<"result">> := []}, State) ->
    State;
handle_response(#{<<"ok">> := false, <<"result">> := Result}, State) ->
    error_logger:info_msg("~nError ~p", [Result]),
    State.

notify_subscribers(_, []) -> ok;
notify_subscribers([], _) -> ok;
notify_subscribers([Head|Tail], Subscribers) ->
    AliveSubscribers = [begin
                            Pid ! Head,
                            Pid
                        end || Pid <- Subscribers, is_process_alive(Pid)],
    notify_subscribers(Tail, AliveSubscribers).

trigger_task(Task) ->
    gen_server:cast(self(), Task),
    ok.
