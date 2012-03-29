var jquery = $;
var lispData = {};
var dataListener = {};
var events = [];
var lisp = io.connect('http://' + window.location.hostname + ':8001');

lisp.on('full', function(objs) {
              console.log("Retrieving objects from Lisp");
              lispData = objs;
              });

lisp.on('update', function(obj) {
              console.log("Update object with id: " + obj._id);
              lispData[obj._id] = obj;
              maybeCallDataListener(obj._id, obj);
              });

lisp.on('event', function(event, args) {
              // console.log("Event: " + event + " with args: " + args);
              window[event].apply(null, args);
              });

lisp.on('funcall', function(fn, args) {
              // console.log("Funcall: " + fn + " with args: " + args);
              eval(fn + ".apply(null, " + args + ")");
              });

lisp.on('eval', function(code) {
              // console.log("Eval: " + code);
              eval(code);
              });

function updateObj(id) {
    var obj;

    if (typeof(id) == 'string') {
        obj = lispData[id];
    }
    else
    {
        obj = id;
    }

    if (obj)
    {
        lisp.emit('update', obj);
    }
}

function print(obj) {
    console.log(obj);
}

function getObj(id) {
    return lispData[id];
}

function addDataListener (id, fn) {
    dataListener[id] = fn;
}

function removeDataListener (id) {
    dataListener[id] = null;
}

function maybeCallDataListener (id, obj) {
    var callback = dataListener[id];

    if (callback) {
        callback.apply(null, [obj]);
    }
}

