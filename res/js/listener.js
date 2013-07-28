var unit = {
    None    : 0,
    Seconds : 1,
    Bytes   : 2
};

function shortUnit(u)
{
    if (u == unit.None) {
        return '';
    } else if (u == unit.Seconds) {
        return 'sec';
    } else if (u == unit.Bytes) {
        return 'B';
    } else {
        return '?';
    }
}


function normalize(v)
{
    if         (v > 1000000000000) {
        return (v / 1000000000000) + 'T';
    } else if  (v > 1000000000) {
        return (v / 1000000000)    + 'G';
    } else if  (v > 1000000) {
        return (v / 1000000)       + 'M';
    } else if  (v > 1000) {
        return (v / 1000)          + 'K';
    } else if  (v == 0) {
        return v + '';
    } else if  (v < 0.000000000001) {
        return (v * 0001000000000) + 'p';
    } else if  (v < 0.000000001) {
        return (v * 1000000000)    + 'n';
    } else if  (v < 0.000001) {
        return (v * 1000000)       + 'μ';
    } else if  (v < 0.001) {
        return (v * 1000)          + 'm';
    } else {
        return v + '';
    }
}

function format(v, u)
{
    return normalize(v) + shortUnit(u);
}

/*----------------------------------------------------------------------
  Counter
----------------------------------------------------------------------*/

function Counter(name)
{
    this.value = null;
    this.unit  = unit.None;

    this.root = document.createElement("tr");
    this.root.className = "counterEntry";
    this.root.onclick   = undefined;

    this.name = document.createElement("td");
    this.name.className = "counterName";
    this.name.innerHTML = name;
    this.root.appendChild(this.name);

    this.val = document.createElement("td");
    this.val.className = "counterValue";
    this.val.innerHTML = "N/A";
    this.root.appendChild(this.val);
}

Counter.prototype.getValue = function()
{
    return this.value;
}

Counter.prototype.setValue = function(v)
{
    this.val.innerHTML = format(v, this.unit);
    this.value = v;
}

/*----------------------------------------------------------------------
  Gauge
----------------------------------------------------------------------*/

function Gauge(name)
{
    this.id    = name;
    this.value = null;
    this.unit  = unit.None;

    this.min   = null;
    this.max   = null;
    this.slope = null;

    this.root = document.createElement("tr");
    this.root.className = "gaugeEntry";
    this.root.onclick   = undefined;

    this.name = document.createElement("td");
    this.name.className = "gaugeName";
    this.name.innerHTML = name;
    this.root.appendChild(this.name);

    this.valueElem = document.createElement("td");
    this.valueElem.className = "gaugeValue";
    this.valueElem.innerHTML = "N/A";
    this.root.appendChild(this.valueElem);

    this.minElem = document.createElement("td");
    this.minElem.className = "gaugeMin";
    this.minElem.innerHTML = "N/A";
    this.root.appendChild(this.minElem);

    this.maxElem = document.createElement("td");
    this.maxElem.className = "gaugeMax";
    this.maxElem.innerHTML = "N/A";
    this.root.appendChild(this.maxElem);

    this.slopeElem = document.createElement("td");
    this.slopeElem.className = "gaugeSlope";
    this.slopeElem.innerHTML = "N/A";
    this.root.appendChild(this.slopeElem);
}

Gauge.prototype.getValue = function()
{
    return this.value;
}

Gauge.prototype.setValue = function(v)
{
    this.min   = Math.min(this.min, v);
    this.max   = Math.max(this.max, v);
    this.slope = v - this.value;
    this.value = v;

    this.valueElem.innerHTML = format(this.value, this.unit);
    this.minElem.innerHTML   = format(this.min  , this.unit);
    this.maxElem.innerHTML   = format(this.max  , this.unit);
    this.slopeElem.innerHTML = format(this.slope, this.unit);
}

/*----------------------------------------------------------------------
  Group
----------------------------------------------------------------------*/

function Group(gid)
{
    this.id       = gid;
    this.counters = {};

    this.root = document.createElement("div");
    this.root.className = "group";

    this.caption = document.createElement("div");
    this.caption.className = "groupCaption";
    var self = this;
    this.caption.onclick   = function () { self.toggle(); };
    this.caption.innerHTML = gid;
    this.root.appendChild(this.caption);

    this.body = document.createElement("table");
    this.body.className = "groupBody";
    this.root.appendChild(this.body);

    this.header = document.createElement("thead");
    this.header.className = "groupHeader";
    this.header.innerHTML = "<tr><th>name</th>\
                          <th>value</th>\
                          <th>min</th>\
                          <th>max</th>\
                          <th>slope</th>\
                          </tr>";
    this.body.appendChild(this.header);
}

Group.prototype.subscribe = function ()
{
    var msg = { Subscribe : this.id };
    sock.send(JSON.stringify(msg));
}

Group.prototype.unsubscribe = function ()
{
    var msg = { Unsubscribe : this.id };
    sock.send(JSON.stringify(msg));
}

Group.prototype.toggle = function ()
{
    if (this.body.style.display == "none") {
        this.subscribe();
        this.body.style.display = "block";
    } else {
        this.unsubscribe();
        this.body.style.display = "none";
    }
}

Group.prototype.remove = function()
{
    var monitor = getElementById("monitor");
    var group   = getElementById(this.id);
    monitor.removeChild(group);
}

Group.prototype.addCounter = function(cid)
{
    // TODO FIXME
    var counter = new Gauge(cid);
    this.body.appendChild(counter.root);
    this.counters[cid] = counter;
    return counter;
}

Group.prototype.getCounter = function(cid)
{
    var counter = this.counters[cid];
    return counter ? counter : this.addCounter(cid);
}

Group.prototype.removeCounter = function()
{
    console.log("not implemented");
}

Group.prototype.update = function(updates)
{
    for (var cid in updates) {
        this.getCounter(cid)
            .setValue(updates[cid]);
    }
}

/*----------------------------------------------------------------------
  Monitor
----------------------------------------------------------------------*/

function Monitor(port)
{
    this.groups = {};
    this.root   = document.getElementById("monitor");
}

Monitor.prototype.addGroup = function(gid)
{
    var group = new Group(gid);
    this.root.appendChild(group.root);
    this.groups[gid] = group;
    return group;
}

Monitor.prototype.getGroup = function(gid)
{
    var group = this.groups[gid];
    return group ? group : this.addGroup(gid);
}

Monitor.prototype.removeGroup = function(gid)
{

}

function handleEvent(m, ev)
{
    var msg = JSON.parse(ev.data);
    if (msg.Update) {
        m.getGroup(msg.Update[0]).update(msg.Update[1]);
    } else if (msg.Remove) {
        m.removeGroup(msg.Remove[0]);
    } else {
        console.log("unknown message " + msg);
    }
}

// for debugging
var sock = undefined;
var moni = null;

function runMonitor(port)
{
    var m = new Monitor(port);
    moni = m;
    var interval = 1000;

    if ("WebSocket" in window) {
        var url = "ws://localhost:" + port;
        var ws  = new WebSocket(url);

        ws.onopen    = function()   { sock = ws; };
        ws.onmessage = function(ev) { handleEvent(m, ev) };
        ws.onclose   = function()   { setTimeout(m.listen, m.interval); };
        ws.onerror   = console.log;
    } else {
        alert("WebSocket NOT supported by your Browser!");
    }
}
