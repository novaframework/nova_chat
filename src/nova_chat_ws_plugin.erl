-module(nova_chat_ws_plugin).

-behaviour(nova_plugin).

-export([pre_ws_request/2,
         post_ws_request/2]).

pre_ws_request(State, Options) ->
    logger:info("State: ~p Options: ~p", [State, Options]),
    {ok, State}.

post_ws_request({_,_, State} = WS, Options) ->
    logger:info("WS: ~p Options: ~p", [WS, Options]),
    {ok, State, Options}.
    
