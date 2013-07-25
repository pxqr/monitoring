function counterBody(id, val)
{
  return id + ": " + val;
}

function add(id, val)
{
  console.log("adding " + id);

  var counter = document.createElement("div");
  counter.setAttribute("id", id);
  counter.innerHTML = counterBody(id, val)

  var monitor = document.getElementById("monitor");
  monitor.appendChild(counter);
}

function refresh(id, val)
{
  console.log("refresh " + id);

  var monitor = document.getElementById("monitor");
  var counter = document.getElementById(id);
  counter.innerHTML = counterBody(id, val);
}

function remove(id)
{
  console.log("removing " + id);

  var monitor = getElementById("monitor");
  var counter = getElementById(id);
  monitor.removeChild(counter);
}

function update(id, val)
{
  var elem = document.getElementById(id);
  if (elem == null) {
    add(id, val);
  } else {
    refresh(id, val);
  }
}

function eventHandler(ev)
{
  var msg = JSON.parse(ev.data);
  console.log("event handler " + msg);

  if (msg.Update) {
    update(msg.Update[0], msg.Update[1]);
  } else if (msg.Remove) {
    remove(msg.Remove[0]);
  } else {
    consol.log("unknown message id " + msg);
  }
}

// TODO leak?
function reconnect(port)
{
  console.log("reconnect " + port)
  var interval = 1000;

  return function () {
     setTimeout(function () { listenEvents(port); }, interval);
  }
}

function listenEvents(port)
{
  if ("WebSocket" in window) {
     var ws = new WebSocket("ws://localhost:" + port);
     ws.onopen = function() {
     // ws.send("Message to send");
     };
     ws.onmessage = eventHandler;
     ws.onclose   = reconnect(port);
     ws.onerror   = console.log
   } else {
     alert("WebSocket NOT supported by your Browser!");
   }
}
