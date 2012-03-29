Ext.require(['*']);
Ext.onReady(function () {
    var dateMenu = Ext.create('Ext.menu.DatePicker');
    var colorMenu = Ext.create('Ext.menu.ColorPicker');
    var fileMenu = Ext.create('Ext.menu.Menu', { id : 'fileMenu', items : [{ 'text' : 'Load data', 'handler' : function () {
        return lisp.emit('event', 'load-data');
    } }, { 'text' : 'Show visualization', 'handler' : showVis }] });
    var prefMenu = Ext.create('Ext.menu.Menu', { id : 'prefMenu', items : [{ 'text' : 'Theme', 'menu' : { 'items' : [{ 'text' : 'Aero Glass', 'checked' : true, 'group' : 'theme' }, { 'text' : 'Vista Black', 'checked' : null, 'group' : 'theme' }, { 'text' : 'Gray Theme', 'checked' : null, 'group' : 'theme' }, { 'text' : 'Default Theme', 'checked' : null, 'group' : 'theme' }] } }, { 'text' : 'Date', 'menu' : dateMenu }, { 'text' : 'Color', 'menu' : colorMenu }] });
    var helpMenu = Ext.create('Ext.menu.Menu', { id : 'helpMenu', items : [{ 'text' : 'About' }] });
    var menuBar = Ext.create('Ext.toolbar.Toolbar', { id : 'menuBar', items : [{ 'text' : 'File', 'menu' : fileMenu }, { 'text' : 'Options', 'menu' : prefMenu }, { 'text' : 'Help', 'menu' : helpMenu }] });
    return menuBar.render('menubar');
});
