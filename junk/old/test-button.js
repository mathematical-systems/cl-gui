Ext.onReady(function(){

	// Make the button Draggable inside  tag
	var initDragZone = function(v) {

		v.dragZone = new Ext.dd.DragZone(Ext.getBody(), {
			getDragData: function(e) {
				// .button-draggable == class of the button you want to drag around
				if(sourceEl = e.getTarget('.button-draggable')) {
					d = sourceEl.cloneNode(true);
					d.id = Ext.id();
					return v.dragData = {
						sourceEl: sourceEl,
						repairXY: Ext.fly(sourceEl).getXY(),
						ddel: d
					}
				}
			},

			onDrag: function(e) {
				// !Important: manually fix the default position of Ext-generated proxy element
				// Uncomment these line to see the Ext issue
				var proxy = Ext.DomQuery.select('*', this.getDragEl());
				proxy[2].style.position = '';
			},

			getRepairXY: function() {
				return this.dragData.repairXY;
			}
		});
	};

        // Make the panel droppable to the button
	var initDropZone = function(g) {
		g.dropZone = new Ext.dd.DropZone(g.body, {

			getTargetFromEvent: function(e) {
				return e.getTarget('#canvas');
			},

			onNodeOver : function(target, dd, e, data){
				return Ext.dd.DropZone.prototype.dropAllowed;
			},

			onNodeDrop : function(target, dd, e, data) {
				// !Important: We assign the dragged element to be set to new drop position
				if(dragEl = Ext.get(data.sourceEl)) {
					dragEl.setXY([e.getPageX(),e.getPageY()]);
				}

				return true;
			}

		});
	};

        // Make the Panel Droppable
	var myPanel = new Ext.Panel({
		width: 500,
		height: 300,
		renderTo: 'canvas',
		bodyStyle: { background: 'yellow' },
		html: 'Drop your button here',
		listeners: {
			render: initDropZone
		}
	});

	var myButton = new Ext.Button({
		width: 30,
		height: 20,
		cls: 'button-draggable',
		text: "Drag me",
		renderTo: Ext.getBody(),
		listeners: {
			render: initDragZone
		}
	});
});
