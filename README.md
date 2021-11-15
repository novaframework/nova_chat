# Nova chat

![screengrab](screengrab.png)

A pubsub chat demo using websockets and [Nova](https://github.com/novaframework/nova). 

In this implementation `nova_pubsub` stores all state in [ets](https://www.erlang.org/doc/man/ets.html), so if a crash occurs and restarts the application all users will need to re-subscribe and start new sockets again.

## API

```bash
 curl -vvv -d '{"topic":"mytopic"}' -X POST http://localhost:8080/user/user1/subscribe
```

This will inform the system that `user1` is now subscribing to topic `mytopic`.

```javascript
// Browser console:
var novachatsocket = new WebSocket("ws://localhost:8080/user/user1/ws");
```

This will set `user1` as online on that socket. You can add more users but don't forget that each user needs to subscribe to a topic using the API.

```javascript
// Browser console:
novachatsocket.send('{"topic":"mytopic", "payload":"This is a message"}');
```


Nova chat will know which user is on the websocket the data is sent over, and will add that user to the payload `user` field. Other users that subscribe to this topic and are online will get a message that looks like this:

```javascript
{"topic":"mytopic",
 "user":"user1",
 "payload":"This is a message"}
```