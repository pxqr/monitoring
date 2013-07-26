function createGroup(gid)
{
    console.log("adding " + gid);

    var gelem = document.createElement("table");
    gelem.setAttribute("id", gid);
    gelem.setAttribute("class", "group");
    gelem.innerHTML = gid;

    var monitor = document.getElementById("monitor");
    monitor.appendChild(gelem);
    return gelem;
}

function removeGroup(gid)
{
    console.log("removing " + gid);

    var monitor = getElementById("monitor");
    var group   = getElementById(gid);
    monitor.removeChild(group);
}

function getGroup(gid)
{
    var gelem = document.getElementById(gid);
    return gelem ? gelem : createGroup(gid);
}

function createCounter(gid, cid)
{
    console.log("adding counter " + cid);

    var celem = document.createElement("tr");
    celem.setAttribute("id", cid);
    celem.setAttribute("class", "counterEntry");
    celem.innerHTML =
        "<td class='counterName'>" + cid + "</td>\
         <td class='counterValue'>   N/A    </td>"

    var gelem = getGroup(gid);
    gelem.appendChild(celem);
    return celem;
}

function getCounterValue(gid, cid)
{
    var celem = document.getElementById(cid);
    console.log("get counter " + celem);
    var counter = celem ? celem : createCounter(gid, cid);
    return counter.getElementsByClassName("counterValue")[0];
}

function updateGroup(gid, counters)
{
    console.log("refresh " + gid);

    for (var cid in counters) {
        var celem = getCounterValue(gid, cid);
        celem.innerHTML = counters[cid];
    }
}

function eventHandler(ev)
{
    var msg = JSON.parse(ev.data);
    console.log("event handler " + msg);

    if (msg.Update) {
        updateGroup(msg.Update[0], msg.Update[1]);
    } else if (msg.Remove) {
        removeGroup(msg.Remove[0]);
    } else {
        consol.log("unknown message " + msg);
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
