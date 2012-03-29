//draw function from https://developer.mozilla.org/en/Drawing_Graphics_with_Canvas
function draw(el) {
  var canvas = el;
  var ctx = canvas.getContext("2d");

  ctx.fillStyle = "red";

  ctx.beginPath();
  ctx.moveTo(30, 30);
  ctx.lineTo(150, 150);
  ctx.bezierCurveTo(60, 70, 60, 70, 70, 150);
  ctx.lineTo(30, 30);
  ctx.fill();
}

Ext.onReady(function(){

canvasPanel = new Ext.Panel({
	title:'Ext JS Canvas Panel'
	,height:200
	,width:400
	,frame:true
	//,renderTo:'ext-canvas-container' //NOTICE: renderTo does not work because the var canvasPanel will not be available in the renderer yet
	,items:{
		xtype: 'box',
		autoEl:{
			tag: 'canvas'
			,height:150
		}
		,listeners:{
			render:{
				scope:this
				,fn:function(){
					draw(canvasPanel.items.items[0].el.dom);
				}
			}
		}
	}
});
canvasPanel.render('ext-canvas-container');

});

