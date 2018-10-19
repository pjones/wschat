class ChatBox extends HTMLElement {
  // Initialize a new chat box.
  constructor() {
    super();

    const shadowRoot = this.attachShadow({mode: 'open'})
    const template = document.getElementById("chat-template");
    shadowRoot.appendChild(template.content.cloneNode(true));
  }

  // The custom element has been added to the DOM.
  connectedCallback() {
    try {
      this.shadowRoot.querySelector('[type="submit"]').disabled = true;

      const host = this.getAttribute("data-host");
      const code = this.getAttribute("data-code");
      const socket = new WebSocket(`wss://${host}/${code}`);

      socket.onopen = () => {
        this.shadowRoot.querySelector('[type="submit"]').disabled = false;
      };

      socket.onclose = () => {
        this.chatConnectionClosed();
      };

      socket.onmessage = (e) => {
        this.receiveChatMessage(JSON.parse(e.data));
      };

      socket.onerror = socket.onclose;

      this.shadowRoot.querySelector("form")
        .addEventListener("submit", (event) => {
          event.preventDefault();
          this.sendChatMessage(socket);
        });
    } catch (e) {
      this.chatConnectionClosed();
      throw e;
    }
  }

  // An incoming message from the WebSocket server:
  receiveChatMessage(msg) {
    const li = document.createElement("li");

    const sender = document.createElement("div");
    sender.classList.add("sender");
    sender.textContent = msg.sender;
    li.appendChild(sender);

    const content = document.createElement("div");
    content.classList.add("content");
    content.textContent = msg.content;
    li.appendChild(content);

    this.shadowRoot.querySelector("ul").appendChild(li);
  }

  // Send a message from the form to the server:
  sendChatMessage(socket) {
    const input = this.shadowRoot.querySelector('[name="content"]');
    const content = input.value.trim();
    if (content.length === 0) return;

    const message = {
      sender: this.getAttribute("data-user"),
      content: content,
    };

    input.value = "";
    socket.send(JSON.stringify(message));
  }

  // The connection is dead:
  chatConnectionClosed() {
    for (let i of Array.from(this.shadowRoot.querySelectorAll("input"))) {
      i.disabled = true;
    }

    this.receiveChatMessage({sender: "browser", content: "Connection closed."});
  }
}

// Register the custom element:
customElements.define("chat-box", ChatBox);
