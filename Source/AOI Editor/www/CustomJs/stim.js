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
		stimulMetaData.removeAttr("originalWidth");
		stimulMetaData.removeAttr("originalHeight");
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
	$('#wiScale').val(0);
	$('#hiScale').val(0);
	stimulMetaData.val(bufferCanvas.attr("src"));
	stimName.val(stimulMetaData.attr("imgName"));
	
	if (!stimulMetaData.prop("isnew")){	
		stimHeight.val(stimulMetaData.attr("originalHeight"));
		stimWidth.val(stimulMetaData.attr("originalWidth"));
	} else {
		stimHeight.val(bufferCanvas.height());
		stimWidth.val(bufferCanvas.width());
	}
	
};
	
imgUpdate = function(e){
	fillInputData();
	OwnCanv.stretchBG(e.target,parseInt(stimWidth.val(),10),
								parseInt(stimHeight.val(),10)
								);
	$(e.target).hide();
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
		stimulMetaData.attr("originalWidth",m.imgWidth);
		stimulMetaData.attr("originalHeight",m.imgHeight);
		stimulMetaData.attr("imgId",m.pictId);
		stimulMetaData.prop("isnew", false).trigger('change');
		console.log("Replace with old");		
	}
		
		
};


zoomIt=function (xf,yf) {
	OwnCanv.setHeight(Math.round(OwnCanv.getHeight() * yf));
	OwnCanv.setWidth(Math.round(OwnCanv.getWidth() * xf));
	if (OwnCanv.backgroundImage) {
		var bi = OwnCanv.backgroundImage;
		bi.width = Math.round(bi.width * xf); bi.height = Math.round(bi.height * yf);
	}
	var objects = OwnCanv.getObjects();
	objects.forEach(function(o){
		if (!o.hidden){
			var scaleX = o.scaleX;
			var scaleY = o.scaleY;
			var left = o.left;
			var top = o.top;

			var tempScaleX = scaleX * xf;
			var tempScaleY = scaleY * yf;
			var tempLeft = left * xf;
			var tempTop = top * yf;

			o.scaleX = tempScaleX;
			o.scaleY = tempScaleY;
			o.left = tempLeft;
			o.top = tempTop;
			o.setCoords();
			OwnCanv.trigger('object:modified', {target: o});
		}
		})
	OwnCanv.renderAll();
	OwnCanv.calcOffset();
}


function setSize(w,h){
	
	var xs=Math.abs(w)/OwnCanv.getWidth();
	var ys=Math.abs(h)/OwnCanv.getHeight();
	
	zoomIt(xs,ys);
	
	stimHeight.val(OwnCanv.getHeight());
	stimWidth.val(OwnCanv.getWidth());
	
	if (!stimulMetaData.prop("isnew")){	
		
		var fScaleData={
					newScale:					"cNewScale"+stimWidth.val()+stimHeight.val(),
					stimId:						parseInt(stimulMetaData.attr("imgId"),10),
					stimWidthShnInput:			parseInt(stimWidth.val(),10),
					stimHeightShnInput:			parseInt(stimHeight.val(),10)
				};
		console.log(fScaleData);			
		shinySend(fScaleData);
	};
}

function zoom(e){
	var xs;
	var ys;
	if($('#ratioFixed').prop("checked")){
		$('#wiScale').val(e.target.valueAsNumber);
		$('#hiScale').val(e.target.valueAsNumber);
	}
	if($('#wiScale').val()>0){xs =parseInt(stimulMetaData.attr("originalWidth"),10)*scaleMarkers[$('#wiScale').val()];}
	else {xs = parseInt(stimulMetaData.attr("originalWidth"),10)/scaleMarkers[$('#wiScale').val()*-1];}
	if($('#hiScale').val()>0){ys = parseInt(stimulMetaData.attr("originalHeight"),10)*scaleMarkers[$('#hiScale').val()];}
	else{ys = parseInt(stimulMetaData.attr("originalHeight"),10)/scaleMarkers[$('#hiScale').val()*-1];}
	setSize(xs,ys);
}

function directResize(e){
	var xs=stimWidth.val();
	var ys=stimHeight.val();
	if($('#ratioFixed').prop("checked")){
		if(e.target.id=='stimWidthShnInput'){
			ys = OwnCanv.getHeight()*(xs/OwnCanv.getWidth());
		}
		if(e.target.id=='stimHeightShnInput'){
			xs =  OwnCanv.getWidth()*(ys/OwnCanv.getHeight());
		}
	}
	setSize(xs,ys);
}



		
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
	var fig = e.data.v;//e.data.figure.val();
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
				$('#wiScale').prop('disabled',true);
		$('#hiScale').prop('disabled',true);
		stimSaveBTN.prop('disabled',false);
	} 
	else {
		stimRepBTN.prop('disabled',false); stimDelBTN.prop('disabled',false);
		$('#wiScale').prop('disabled',false);
		$('#hiScale').prop('disabled',false);
		stimSaveBTN.prop('disabled',true);
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
	AOIGroup.hide();
	
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


	
	// $('#RectAOI').on('click', {figure:AOIGroup.val('rect')}, addFigure);
	$('#RectAOI').on('click', {figure:AOIGroup,v:"rect"}, addFigure);
		
	$('#CircleAOI').on('click', {figure:AOIGroup, v:"circle"}, addFigure);

	stimulEditorPanel.on('click',{el:stimulEditor,itm:$('.clickerItem')},toggleContainer);
	AOIEditorPanel.on('click',{el:AOIEditor,itm:$('.clickerItem'),isAOI:true},toggleContainer);
	AOIsetEditorPanel.on('click',{el:AOIsetEditor,itm:$('.clickerItem'),isAOI:true},toggleContainer);

	bufferCanvas.on('load',imgUpdate);

	Shiny.addCustomMessageHandler("replaceImgCallbackHandler",imgReplaceMsgH);
	Shiny.addCustomMessageHandler("resetInputsImgCallbackHandler",clearIOMsgH);
	
	$('#drawPolygon').on('click',polygonByMouse)

	$('#wiScale').on('change',zoom)
	$('#hiScale').on('change',zoom)

	stimHeight.on('change',directResize)
	stimWidth.on('change',directResize)
	
	clearScene();


	stimulEditor.show();
	AOIEditor.hide();
	AOIsetEditor.hide();
	
	
	
	
	
	
});