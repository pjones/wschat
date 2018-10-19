A Simple WebSocket Chat Server
==============================

This package includes a single executable: `wschat`.  It's an
extremely simple chat server using WebSockets.

The primary goal of this package is to provide a WebSocket server for
students who are learning JavaScript so they have a real server to
experiment with.

Connecting from JavaScript
--------------------------

Before connecting you will need two pieces of information: the
server's host name and an access code.  For this example we'll assume
the following information:

| Needed Information  | Example Value |
| ------------------- | ------------- |
| hostname            | example.com   |
| access code         | abc123        |

Now we can construct a URL and open a connection:

```javascript
const socket = new WebSocket("wss://example.com/abc123");
```

*NOTE:* if your server doesn't support SSL/TLS you will have to
replace `wss://` with `ws://`.

All chat messages that are sent and received are JSON encoded objects
that look like this:

```javascript
{
  "sender": "name of message sender",
  "content": "the content of the message"
}
```

Here's an example of sending a message to the server:

```javascript
const message = {
  sender: "Alice",
  content: "Hello!"
};

socket.send(JSON.stringify(message));
```

A complete example using web components can be found in the `examples`
directory.
