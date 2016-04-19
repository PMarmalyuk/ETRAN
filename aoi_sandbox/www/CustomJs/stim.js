waitForDraw=function(){
	if(drawFinished==true){}
	else{	setTimeout(waitForDraw,250)	}
}

resetIO = function(){
		userCanvas.hide();
		bufferCanvas.attr("src","");
		
		stimWidth.val(0);
		stimHeight.val(0);
		stimName.val("");
		
		stimRepBTN.prop('disabled',true);
		stimDelBTN.prop('disabled',true);
		stimSaveBTN.prop('disabled',true);
		
		stimulMetaData.val("");
		stimulMetaData.attr("imgName","");
		stimulMetaData.removeAttr("imgId");
		stimulMetaData.prop("isnew",true);
		
		stimFileSrc.val("");
		stimFileSrcProgress.hide();
	};
		
loadStimFile = function() {
		var reader = new FileReader();
		reader.readAsDataURL(this.files[0]);
		reader.onload = function(){
			var m={src:reader.result,name:stimFileSrc.val(),isnew:true};
			imgReplaceMsgH(m);
		}
};
	
sendInputData = function(){
	var fImgData={		imgSrc:						stimulMetaData.val(),
						stimId:						parseInt(stimulMetaData.attr("imgId"),	10),
						stimWidthShnInput:stimWidth.val(),  stimHeightShnInput:stimHeight.val(), stimNameShnInput:stimName.val()
				};
	shinySend(fImgData);
};
	
	
fillInputData = function(){
	stimHeight.val(bufferCanvas.height());
	stimWidth.val(bufferCanvas.width());
	
	stimulMetaData.val(bufferCanvas.attr("src"));
	stimName.val(stimulMetaData.attr("imgName"));
	};
	
imgUpdate = function(e){
	OwnCanv.reloadBG(e.target);
	fillInputData();
	$(e.target).hide();
};
		
addDrawing = function(fig){
	var figToAdd = fig;
	var rType = "Polyhedron";
	var rData = figToAdd.getRDisp();
	OwnCanv.add(figToAdd); 
	
	if (AOIName.val()==''){AOIName.val(rType)};
	var o = {name:AOIName.val(), rType:rType, rData:rData};
	AOIcol.add(o);
	OwnCanv.setActiveObject(OwnCanv.getObjects().last());
	AOIListContent.find("#bootstrap-duallistbox-nonselected-list_duallistbox").children().last().prop("selected",true);//.trigger("click");
	$("button.move").trigger("click");
	AOIListContent.find("#bootstrap-duallistbox-selected-list_duallistbox").children().last().prop("selected",true).trigger("click");
	//OwnCanv.getObjects().last().focus();
	
}
		
addFigure = function(e){
	var fig = e.data.figure.val();
	var commonParam={fill: 'red', opacity: 0.5 , fill: 'red', opacity: 0.5, selectable: true};
	
	OwnCanv.getObjects().forEach(function(element){element.hide()})
	if(fig=="rect"){
		var figParams = [{x: 0, y: 0},{x: 60, y: 0},{x: 60, y: 60},{x: 0, y: 60}];
		var figToAdd = new fabric.Polygon(figParams);
		var rType = "Polyhedron";
		var rData = figToAdd.getRDisp();
	}
	if(fig=="circle"){
		var figParams = {rx: 50, ry: 50};
		var figToAdd = new fabric.Ellipse(figParams);
		var rType = "Ellipse";
		var rData = figToAdd.getRDisp();
	}

	figToAdd.set(commonParam); OwnCanv.add(figToAdd); 
	
	if (AOIName.val()==''){AOIName.val(rType)};
	var o = {name:AOIName.val(), rType:rType, rData:rData};
	AOIcol.add(o);
	OwnCanv.setActiveObject(OwnCanv.getObjects().last());
	AOIListContent.find("#bootstrap-duallistbox-nonselected-list_duallistbox").children().last().prop("selected",true);
	$("button.move").trigger("click");
	AOIListContent.find("#bootstrap-duallistbox-selected-list_duallistbox").children().last().prop("selected",true).trigger("click");
};
	
defineModifyAct = 	function (){
	if ($(this).prop("isnew")){
		stimRepBTN.prop('disabled',true); stimDelBTN.prop('disabled',true);
		stimSaveBTN.prop('disabled',false);
	} 
	else {
		stimRepBTN.prop('disabled',false); stimDelBTN.prop('disabled',false);
		stimSaveBTN.prop('disabled',true);
	}	
};	

imgReplaceMsgH =function(m){
	bufferCanvas.attr("src",m.src);
	stimName.val(m.name);
	stimulMetaData.attr("imgName",m.name);

	if(m.isnew){
		stimulMetaData.removeAttr("imgId");
		stimulMetaData.prop("isnew",true).trigger("change"); 
		console.log("Replace with new");
	}
	else{
		stimulMetaData.attr("imgId",m.pictId);
		stimulMetaData.prop("isnew", false).trigger('change');
		console.log("Replace with old");		
	} 
};
	
clearIOMsgH = function(m){
	 resetIO();
	}	
	

toggleContainer = function(e){
	if(e.data.el.css("display")=="none") {
		e.data.itm.slideUp('slow');
		e.data.el.slideDown('slow');
		if(e.data.isAOI) {AOIListContent.fadeIn()} else {AOIListContent.fadeOut()};
	} 
	else {e.data.el.slideUp('slow')}
}	


polygonByMouse=function(){
	var mode="add";
	var currentShape=new fabric.Polygon()
	OwnCanv.getObjects().forEach(function(element){element.hide()})
	OwnCanv.renderAll();
	
OwnCanv.observe("mouse:move", function (event) {
    var pos = OwnCanv.getPointer(event.e);
    if (mode === "edit" && currentShape) {
        var points = currentShape.get("points");
        points[points.length - 1].x = pos.x - currentShape.get("left");
        points[points.length - 1].y = pos.y - currentShape.get("top");
        currentShape.set({
            points: points
        });
        OwnCanv.renderAll();
    }
});

OwnCanv.observe("mouse:down", function (event) {
	var pos = OwnCanv.getPointer(event.e);

    if (mode === "add") {
        var polygon = new fabric.Polygon([{
            x: pos.x,
            y: pos.y
        }, {
            x: pos.x + 0.5,
            y: pos.y + 0.5
        }], {
            fill: 'red',
            opacity: 0.5,
            selectable: false
        });
        currentShape = polygon;
        OwnCanv.add(currentShape);
        mode = "edit";
    } else if (mode === "edit" && currentShape && currentShape.type === "polygon") {
        var points = currentShape.get("points");
        points.push({
            x: pos.x - currentShape.get("left"),
            y: pos.y - currentShape.get("top")
        });
        currentShape.set({
            points: points
        });
        OwnCanv.renderAll();
    }
});

function fixPoly(poly) {
  var oldC = poly.getCenterPoint();
  poly._calcDimensions();
  poly.set({
    left: poly.get('left') + poly.get("minX"),
    top: poly.get('top') + poly.get("minY")
  });

  var pCenter = poly.getCenterPoint();
  poly.get("points").forEach(function(p){
    p.x -= pCenter.x - oldC.x;
    p.y -= pCenter.y - oldC.y
  });
  poly.setCoords();
};

fabric.util.addListener(window, 'keyup', function (e) {
    if (e.keyCode === 27) {
        if (mode === 'edit' || mode === 'add') {
            mode = 'normal';
            /*
			currentShape.set({
                selectable: true
            });
			*/
			fixPoly(currentShape)
			addDrawing(currentShape);
			currentShape.remove();
			console.log(OwnCanv.getObjects())
			OwnCanv.off("mouse:move")
			OwnCanv.off("mouse:down")
        } else {
            mode = 'add';
        }
        currentShape = null;
    }
})
	
}

	
$(document).ready(function(){
	OwnCanv = new fabric.Canvas('imgEditorCanvas', { isDrawingMode: false }  );
	AOIListContent.hide();
	stimRepBTN.prop('disabled',true);
	stimDelBTN.prop('disabled',true);
	stimSaveBTN.prop('disabled',true);
	//renameAOIBTN.prop('disabled',true);
	
	$.fn.extend({
		en : function(){
				window.renameAOIBTN.prop('disabled',false);
				window.AOIparams.slideDown('slow');
				window.AOIName.val(window.AOIcol.getCurrent().name);
		},
	
		ds : function(){
				window.renameAOIBTN.prop('disabled',true);
				window.AOIparams.slideUp('slow');
				window.AOIName.val("");}
	});
	
	AOIparams.ds();

	AOIGroup.val($("input[name='aoigroup']:checked").val());

	stimFileSrc.on('change', loadStimFile);
	stimulMetaData.on('change', defineModifyAct);
	$("input[name='aoigroup']").on('change',{p:AOIGroup},function(e){e.data.p.val($("input[name='aoigroup']:checked").val())})

	stimSaveBTN.on('click', sendInputData);
	stimRepBTN.on('click', sendInputData);
	stimDelBTN.on('click', sendInputData);

	addAOIBTN.on('click', {figure:AOIGroup}, addFigure);

	stimulEditorPanel.on('click',{el:stimulEditor,itm:$('.clickerItem')},toggleContainer);
	AOIEditorPanel.on('click',{el:AOIEditor,itm:$('.clickerItem'),isAOI:true},toggleContainer);
	AOIsetEditorPanel.on('click',{el:AOIsetEditor,itm:$('.clickerItem'),isAOI:true},toggleContainer);

	bufferCanvas.on('load',imgUpdate);

	Shiny.addCustomMessageHandler("replaceImgCallbackHandler",imgReplaceMsgH);
	Shiny.addCustomMessageHandler("resetInputsImgCallbackHandler",clearIOMsgH);
	
	$('#drawPolygon').on('click',polygonByMouse)



	stimulEditor.show();
	AOIEditor.hide();
	AOIsetEditor.hide();
});