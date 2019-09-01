#{prefix => "",
  security => false,
  routes => [
            {"/topic/:topic", {nova_chat_main_controller, topic}},
	     {"/user/:user/subscribe", {nova_chat_main_controller, subscribe}},
	     {"/user/:user/ws", nova_chat_ws, #{protocol => ws,
					        idle_timeout => 30000}}
           ],
 statics => [
             {"/assets/[...]", "assets"}
            ]
}.
