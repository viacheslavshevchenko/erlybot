-module(erlybot_pooling).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {name        = null,
                token       = null,
                offset      = 0,
                timeout     = 60,
                args        = #{},
                server      = null,
                pooling_ref = null,
                timer_ref   = null,
                subscribers = ordsets:new()}).


%% API.
start_link(#{name := Name} = Args) ->
	gen_server:start_link({local, Name}, ?MODULE, Args, []).

init(#{name := Name, token := Token, server := Server, opts := Opts}) ->
    inets:start(httpc, [{profile, Name}]),
    #{subscribers := Subscribers} = Opts,
	{ok, #state{name = Name, 
                token = Token,
                server = Server,
                args = maps:remove(subscribers, Opts), 
                subscribers = ordsets:from_list(Subscribers),
                timer_ref = erlang:send_after(0, self(), pool)}}.

handle_call(subscribe, {Pid, _}, #state{subscribers = Subscribers} = State) ->
    {reply, ok, State#state{subscribers = ordsets:add_element(Pid, Subscribers)}};
handle_call(unsubscribe, {Pid, _}, #state{subscribers = Subscribers} = State) ->
    {reply, ok, State#state{subscribers = ordsets:del_element(Pid, Subscribers)}};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({http, {FromRef, stream, Json}}, #state{pooling_ref = Ref} = State) when (Ref =:= FromRef) ->
    Msg = jsx:decode(Json, [return_maps]),
    {noreply, (handle_response(Msg, State))#state{pooling_ref = null, timer_ref = erlang:send_after(0, self(), pool)}};
handle_info(pool, State = #state{name = Name, server = Server, token = Token}) ->
    Url = erlybot:get_url(Server, Token, <<"getUpdates">>),
    Body = jsx:encode(#{<<"offset">> => State#state.offset,
                        <<"timeout">> => State#state.timeout}),
    case do_request(Url, Body, Name) of
        {ok, NewRef} ->
            {noreply, State#state{pooling_ref = NewRef, timer_ref = null}};
        {error, Reason} ->
            error_logger:error_report("Error due execute request ~p", [Reason]),
            {noreply, State#state{pooling_ref = null, timer_ref = erlang:send_after(1000, self(), pool)}}
    end;
handle_info(_Msg, State) ->
%%    error_logger:info_msg("~nInfo msg ~p", [_Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal
do_request(Url, Body, Profile) ->
    Options = [{sync, false},
               {full_result, true},
               {stream, self},
               {body_format, binary}],
    case httpc:request(post, {Url, [], "application/json", Body}, [], Options, Profile) of
        {ok, Ref} ->
            {ok, Ref};
        {error, Reason} ->
            {error, Reason}
    end.

handle_response(#{<<"ok">> := true, <<"result">> := #{<<"update_id">> := Id} = Result}, State) ->
    notify_subscribers([Result], State#state.subscribers),
    State#state{offset = Id+1};
handle_response(#{<<"ok">> := true, <<"result">> := [_|_] = ListOfMsgs}, State) ->
    #{<<"update_id">> := LastUpdId} = lists:last(ListOfMsgs),
    notify_subscribers(ListOfMsgs, State#state.subscribers),
    State#state{offset = LastUpdId+1};
handle_response(#{<<"ok">> := true, <<"result">> := []}, State) ->
    State;
handle_response(#{<<"ok">> := false, <<"result">> := Result}, State) ->
    error_logger:info_msg("~nError ~p", [Result]),
    State.

notify_subscribers(_, []) -> ok;
notify_subscribers([], _) -> ok;
notify_subscribers([Head|Tail], Subscribers) ->
    [Pid ! Head || Pid <- Subscribers, is_process_alive(Pid)],
    notify_subscribers(Tail, Subscribers).

