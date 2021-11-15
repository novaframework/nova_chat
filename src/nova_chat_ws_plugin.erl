-module(nova_chat_ws_plugin).

-behaviour(nova_plugin).

-export([pre_ws_request/2,
         post_ws_request/2,
         plugin_info/0]).

pre_ws_request(State, Options) ->
    logger:info("State: ~p Options: ~p", [State, Options]),
    {ok, State}.

post_ws_request(State, Options) ->
    logger:info("WS: ~p Options: ~p", [State, Options]),
    {ok, State}.
    
plugin_info() ->
  {<<"nova_chat_ws_plugin">>, <<"0.1.0">>, <<"Add pre/post messages to WS events">>, []}.


