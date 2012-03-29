var labelType = 'Native';
var useGradients = true;
var nativeTextSupport = true;
var animate = true;
var data;
var fd;
function createVis() {
    return new $jit.ForceDirected({ injectInto : 'infovis', width : 1200, height : 800, Navigation : { enable : true, panning : 'avoid nodes', zooming : 10 }, Node : { overridable : true }, Edge : { overridable : true, color : '#23A4FF', lineWidth : 0.4 }, Label : { type : labelType, size : 10, style : 'bold', color : '#000' }, Tips : { enable : true, onShow : function (tip, node) {
        var count = 0;
        node.eachAdjacency(function () {
            return ++count;
        });
        return tip.innerHTML = '<div class="tip-title">' + node.name + '</div>' + '<div class="tip-text"><b>connections:</b> ' + count + '</div>';
    } }, Events : { enable : true, type : 'Native', onMouseEnter : function () {
        return fd.canvas.getElement().style.cursor = 'move';
    }, onMouseLeave : function () {
        return fd.canvas.getElement().style.cursor = '';
    }, onDragMove : function (node, eventInfo, e) {
        var pos = eventInfo.getPos();
        node.pos.setc(pos.x, pos.y);
        return fd.plot();
    }, onTouchMove : function (node, eventInfo, e) {
        $jit.util.event.stop(e);
        return this.onDragMove(node, eventInfo, e);
    }, onClick : function (node) {
        if (!node) {
            return null;
        };
        var html = '<h4>' + node.name + '</h4><b> connections:</b><ul><li>';
        var list = [];
        node.eachAdjacency(function (adj) {
            return list.push(adj.nodeTo.name);
        });
        return $jit.id('inner-details').innerHTML = html + list.join('</li><li>') + '</li></ul>';
    } }, iterations : 200, levelDistance : 130, onCreateLabel : function (domElement, node) {
        domElement.innerHTML = node.name;
        var style1 = domElement.style;
        style1.fontSize = '0.8em';
        return style1.color = '#ddd';
    }, onPlaceLabel : function (domElement, node) {
        var style2 = domElement.style;
        var left3 = parseInt(style2.left);
        var top4 = parseInt(style2.top);
        var w = domElement.offsetWidth;
        style2.left = left3 - w / 2 + 'px';
        style2.top = top4 + 10 + 'px';
        return style2.display = '';
    } });
};
function showVis() {
    fd = createVis();
    for (var d = null, _js_idx5 = 0; _js_idx5 < lispData.length; _js_idx5 += 1) {
        d = lispData[_js_idx5];
        if (d && d.graph) {
            data = d.graph;
            break;
        };
    };
    fd.loadJSON(data);
    return fd.computeIncremental({ iter : 40, property : 'end', onStep : function (perc) {
        return null;
    }, onComplete : function () {
        return fd.animate({ modes : ['linear'], transition : $jit.Trans.Elastic.easeOut, duration : 2500 });
    } });
};
