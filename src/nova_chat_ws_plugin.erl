-module(nova_chat_ws_plugin).

-behaviour(nova_plugin).

-export([pre_ws_request/2,
         post_ws_request/2,
         plugin_info/0]).

pre_ws_request(State, _Options) ->
    logger:info("pre_ws_request!"),
    {ok, State}.

post_ws_request(State, _Options) ->
    logger:info("post_ws_request!"),
    {ok, State}.
    
plugin_info() ->
  {<<"nova_chat_ws_plugin">>, <<"0.1.0">>, <<"Add pre/post messages to WS events">>, []}.


