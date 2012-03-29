var connect  = require('connect'),
    socketio = require('socket.io'),
    net      = require('net'),
    url      = require('url'),
    path     = require('path'),
    fs       = require('fs'),
    repl     = require('repl');

var http_server_port      = 8000,
    websocket_server_port = 8001,
    tcp_server_port       = 8002;

// http server
var http_server = connect.createServer(
    connect.favicon(),
    connect.logger(),
    connect.static(process.cwd()));

http_server.listen(http_server_port);

// create websocket server
var websocket_server = socketio.listen(websocket_server_port);

websocket_server.sockets.on('connection', function(socket) {
                                console.log("Websocket connection established");
                                // transfer all data at connection
                                socket.emit('full', lispData);
                                
                                socket.on('update', function(data) {
                                              lispData[data._id] = data;
                                              lisp && lisp.write(JSON.stringify({type: 'update', data: data}) + '\n');
                                              });
                                socket.on('event', function(event, args) {
                                              lisp && lisp.write(JSON.stringify({type: 'event', event: event, args: args}) + '\n');
                                              });
                                socket.on('eval', function(code) {
                                              lisp && lisp.write(JSON.stringify({type: 'eval', code: code}) + '\n');
                                              });
                                socket.on('disconnect', function() {
                                              console.log("Websocket connection closed");
                                              });
                            });

console.log("Websocket Server running at port " + websocket_server_port);

// create tcp server
var message = '';
var lisp = null;                       // currently only 1 lisp per server
var lispData = [];
var tcp_server = net.createServer(
    function(socket) {
        socket.addListener("connect", function() {
                               socket.setEncoding("utf8");
                               socket.setNoDelay(true);
                               socket.setKeepAlive(true);
                               console.log("TCP connection from " + socket.remoteAddress + " established");
                               lisp = socket;
                           });
        
        socket.addListener("data", function(data) {
                               console.log("Lisp-side message: " + data);
                               message += data;
                               var newlineIndex = message.indexOf('\n');
                               if (newlineIndex !== -1) {
                                   var oneMessage = message.slice(0, newlineIndex);
                                   message = message.slice(newlineIndex + 1);
                                   try {
                                       var json = JSON.parse(oneMessage);
                                   }
                                   catch (e) {
                                       console.log("Not JSON parsable");
                                       return;
                                   }
                                   
                                   switch (json.type)
                                   {
                                   case 'update':
                                       lispData[json.data._id] = json.data;
                                       websocket_server.sockets.emit('update', json.data);
                                       break;
                                   case 'event':
                                       websocket_server.sockets.emit('event', json.event, json.args);
                                       break;
                                   case 'eval':
                                       websocket_server.sockets.emit('eval', json.code);
                                       break;
                                   case 'full':
                                       lispData = json.data;
                                       websocket_server.sockets.emit('full', lispData);
                                       break;
                                   default:
                                       console.log("Unknown message from Lisp");
                                   };
                               }
                           });
        
        socket.addListener("close", function() {
                               console.log("TCP connection from " + socket.remoteAddress + " closed");
                               lisp = null;
                           });
    });

tcp_server.listen(tcp_server_port,
                  function() {
                      console.log("TCP Server running at port " + tcp_server_port);
                      });


// start repl
repl.start('nodejs> ');
