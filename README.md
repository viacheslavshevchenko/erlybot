# erlybot
Telegram api wrapper
---
### Build
```bash
rebar3 get-deps
rebar3 compile
```
### Start
```
1> application:ensure_all_started(erlybot).
```
### Work
```
2> erlybot:get_me(bot1).
#{<<"ok">> => true,
  <<"result">> =>
      #{<<"first_name">> => <<"***">>,
        <<"id">> => ***,<<"is_bot">> => true,
        <<"username">> => <<"***">>}}
```
### Long pooling update
```
3> erlybot:start_pooling_and_subscribe(bot1).
{ok,<0.109.0>}
4> flush().
Shell got #{<<"message">> =>
                #{<<"chat">> =>
                      #{<<"first_name">> => <<"***">>,
                        <<"id">> => ***,
                        <<"last_name">> => <<"***">>,
                        <<"type">> => <<"private">>,
                        <<"username">> => <<"***">>},
                  <<"date">> => ***,
                  <<"entities">> =>
                      [#{<<"length">> => 6,<<"offset">> => 0,
                         <<"type">> => <<"bot_command">>}],
                  <<"from">> =>
                      #{<<"first_name">> => <<"***">>,
                        <<"id">> => ***,<<"is_bot">> => false,
                        <<"language_code">> => <<"en">>,
                        <<"last_name">> => <<"***">>,
                        <<"username">> => <<"***">>},
                  <<"message_id">> => 177,<<"text">> => <<"/start">>},
            <<"update_id">> => 61975375}
ok
```
