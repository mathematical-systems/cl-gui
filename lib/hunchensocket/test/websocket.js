if ("WebSocket" in window) {
  var ws = new WebSocket("ws://localhost:8000");
  ws.onopen = function() {
    // Web Socket is connected. You can send data by send() method.
    ws.send("message to send");
  };
  ws.onmessage = function (evt) { console.log(evt.data); };
  ws.onclose = function() { console.log("websocket is closed."); };
} else {
    alert("Websocket not supported!");
}
