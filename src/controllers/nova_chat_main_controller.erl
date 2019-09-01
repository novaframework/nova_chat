-module(nova_chat_main_controller).
-export([
	 topic/1,
	 subscribe/1
        ]).

topic(#{method := <<"PUT">>,
        bindings := #{topic := Topic}} = Req) ->
    io:format("~p~n", [Req]),
    {json, [{message, "Topic!"}]}.

subscribe(#{method := <<"POST">>,
	    bindings := #{user := User}} = Req) ->
    {ok, Data, _} = cowboy_req:read_body(Req),
    #{<<"topic">> := Topic} = jsone:decode(Data),
    nova_pubsub:subscribe(User, Topic),
    {json, [{message, "Subscribe!"}]};
subscribe(Req) ->
    io:format("~p", [Req]),
    {json, <<"default">>}.

    
