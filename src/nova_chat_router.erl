-module(nova_chat_router).
-export([
         routes/1
        ]).

routes(_Env) ->
    [#{prefix => "",
       security => false,
       routes => [
                  {"/topic/:topic", {nova_chat_main_controller, topic}, #{methods => [put]}},
                  {"/user/:user/subscribe", {nova_chat_main_controller, subscribe}, #{methods => [post]} },
                  {"/user/:user/ws", nova_chat_ws, #{protocol => ws, idle_timeout => 30000}},
                  {"/", {nova_chat_main_controller, index}, #{methods => [get]}}
                 ],
       statics => [
                   {"/assets/[...]", "assets"}
                  ]
      }].