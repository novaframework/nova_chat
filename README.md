# Nova chat

Is a pubsub chat using websockets and [Nova](https://github.com/novaframework/nova) this is a first experimental what we can do with websockets and Nova.

nova pubsub will have everything in ets so if nova_pubsub crash and restart all users need to subscribe again and also start a new socket.

## API

```bash
 curl -vvv -d '{"topic":"mytopic"}' -X POST http://localhost:8080/user/user1/subscribe
```

This api will tell the system that user1 is subscribing to topic `mytopic`.

```javascript
var novachatsocket = new WebSocket("ws://localhost:8080/user/user1/ws");
```

This can be done in a developer tool in a browser.

This will set user1 as online on that socket. You can add more user but don't forget that each user need to subscribe to a topic. using the api for subscribe.

In browser you can publsih with:

```js
novachatsocket.send('{"topic":"mytopic", "payload":"This is a message"}');
```

Nova chat will know what user it is depending on the websocket that the data is sent on so it will add the user field. Other users that subscribe on this and is online will get a message that looks like this:

```javascript
{"topic":"mytopic",
 "user":"user1",
 "payload":"This is a message"}
```