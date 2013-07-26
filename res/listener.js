var sock = null;

function unsubscribeGroup(gid)
{
    var msg = { Unsubscribe : gid };
    sock.send(JSON.stringify(msg));
}

function subscribeGroup(gid)
{
    var msg = { Subscribe : gid };
    sock.send(JSON.stringify(msg));
}

function toggleGroup(gid)
{
    var gbody = document.getElementById(gid);
    if (gbody.style.display == "none") {
        subscribeGroup(gid);
        gbody.style.display = "block";
    } else {
        unsubscribeGroup(gid);
        gbody.style.display = "none";
    }
}

function createGroup(gid)
{
    var gelem = document.createElement("div");
    gelem.setAttribute("class", "group");

    var gcap = document.createElement("div");
    gcap.setAttribute("class", "groupCaption");
    gcap.setAttribute("onclick", "toggleGroup('" + gid + "')");
    gcap.innerHTML = gid;
    gelem.appendChild(gcap);

    var gbody = document.createElement("table");
    gbody.setAttribute("id", gid);
    gelem.appendChild(gbody);

    var monitor = document.getElementById("monitor");
    monitor.appendChild(gelem);

    return gbody;
}

function removeGroup(gid)
{
    var monitor = getElementById("monitor");
    var group   = getElementById(gid);
    monitor.removeChild(group);
}

function getGroup(gid)
{
    var gelem = document.getElementById(gid);
    return gelem ? gelem : createGroup(gid);
}

function toggleGraph(cid)
{
    var celem = document.getElementById(cid);
}

function createCounter(gid, cid)
{
    var celem = document.createElement("tr");
    celem.setAttribute("id", cid);
    celem.setAttribute("class", "counterEntry");
    celem.setAttribute("onclick", "toggleGraph('" + cid + "')");
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
    var counter = celem ? celem : createCounter(gid, cid);
    return counter.getElementsByClassName("counterValue")[0];
}

function updateGroup(gid, counters)
{
    for (var cid in counters) {
        var celem = getCounterValue(gid, cid);
        celem.innerHTML = counters[cid];
    }
}

function eventHandler(ev)
{
    var msg = JSON.parse(ev.data);
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
            sock = ws;
        };
        ws.onmessage = eventHandler;
        ws.onclose   = reconnect(port);
        ws.onerror   = console.log
    } else {
        alert("WebSocket NOT supported by your Browser!");
    }
}
