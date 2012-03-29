var jquery = $;
var lispData = {};
var events = [];
var lisp = io.connect('http://' + window.location.hostname + ':8001');

lisp.on('full', function(objs) {
              console.log("Retrieving " + objs.length + " objects from Lisp");
              lispData = objs;
              });

lisp.on('update', function(obj) {
              console.log("Update object with id: " + obj._id);
              lispData[obj._id] = obj;
              });

lisp.on('event', function(event, args) {
              // console.log("Event: " + event + " with args: " + args);
              window[event].apply(null, args);
              });

lisp.on('eval', function(code) {
              // console.log("Eval: " + code);
              eval(code);
              });

function update_obj(id) {
    var obj = lispData[id];
    if (obj)
    {
        lisp.emit('update', obj);
    }
}

function print(obj) {
    console.log(obj);
}


