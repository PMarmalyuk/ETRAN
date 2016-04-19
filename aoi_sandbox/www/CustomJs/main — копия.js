	function labelMsg(label,Obj){
		console.log(label);
		console.log(Obj);
	};

	function resetIO(){
		$('#imgEditorCanvas').hide();
		$('#hiddenImgBuffer').attr("src","");
		
		$('#stimWidthShnInput').val(0);
		$('#stimHeightShnInput').val(0);
		$('#stimNameShnInput').val("");
		
		$("#repStim").prop('disabled',true);
		$("#delStim").prop('disabled',true);
		$("#saveStim").prop('disabled',true);
		
		$('#imgMetaDataStorage').val("");
		$("#imgMetaDataStorage").attr("imgName","");
		$("#imgMetaDataStorage").removeAttr("imgId");
		$("#imgMetaDataStorage").prop("isnew",true);
		
		$('#stimImgShnFileSource').val("");
		$('#stimImgShnFileSource_progress').hide();
	};
		
	function loadStimFile() {
		var reader = new FileReader();
		reader.readAsDataURL(this.files[0]);
		reader.onload = function(){
			var m={src:reader.result,IName:$('#stimImgShnFileSource').val(),isnew:true};
			imgReplaceMsgH(m);
		};	
	};
	
	function sendInputData(){
	var fImgData={		imgSrc:						$('#imgMetaDataStorage').val(),
						stimId:						parseInt($("#imgMetaDataStorage").attr("imgId"),	10),
						stimWidthShnInput:			$('#stimWidthShnInput').val(),
						stimHeightShnInput:			$('#stimHeightShnInput').val(),
						stimNameShnInput:  			$('#stimNameShnInput').val()
					};
		Shiny.shinyapp.sendInput(fImgData);
	};
	
	function fillInputData(PushValues){
		if(PushValues){
			$("#stimHeightShnInput").val($('#hiddenImgBuffer').height()).trigger('change');
			$("#stimWidthShnInput").val($('#hiddenImgBuffer').width()).trigger('change');
			
			$("#imgMetaDataStorage").val($('#hiddenImgBuffer').attr("src")).trigger('change');
			$('#stimNameShnInput').val($("#imgMetaDataStorage").attr("imgName")).trigger('change');
		}
		else {
			$("#stimHeightShnInput").val($('#hiddenImgBuffer').height());
			$("#stimWidthShnInput").val($('#hiddenImgBuffer').width());
			
			$("#imgMetaDataStorage").val($('#hiddenImgBuffer').attr("src"));
			$('#stimNameShnInput').val($("#imgMetaDataStorage").attr("imgName"));
		}
	};
	
	function imgUpdate(e){
		//$('#imgEditorCanvas').fadeOut();
		//$(e.Canvas).fadeOut();
		e.data.cv.reloadBG(e.target);
		fillInputData(false);
		$(e.target).hide();
		//$('#imgEditorCanvas').fadeIn();
	};
	
	
	function addFigure(e){
		var fig = e.data.figure.val();
		var commonParam={fill: 'red', opacity: 0.5 , left: 50, top: 50, fill: 'red', opacity: 0.5, selectable: true};
		
		if(fig=="rect"){
			var figParams = [{x: 0, y: 0},{x: 60, y: 0},{x: 60, y: 60},{x: 0, y: 60}];
			var figToAdd = new fabric.Polygon(figParams);
		}
		
		if(fig=="circle"){
			var figParams = {rx: 50, ry: 50};
			var figToAdd = new fabric.Ellipse(figParams);
		}
		
		figToAdd.set(commonParam);
		e.data.c.add(figToAdd).setActiveObject(figToAdd);
		
		var id = e.data.c.getObjects().indexOf(figToAdd);
		if (id>-1) {
			var o = {	fabricID:	id,
						fabricView:	OwnCanv.toJSON().objects[id],
						name:		OwnCanv.toJSON().objects[id].type};
		
			AOIcol.add(o);
			console.log(AOIcol.view());}
	};
	
	function defineModifyAct(e){
		if ($(this).prop("isnew")){
			$("#repStim").prop('disabled',true);
			$("#delStim").prop('disabled',true);
			$("#saveStim").prop('disabled',false);
			console.log("isnew");
		} 
		else 
		{
			$("#repStim").prop('disabled',false);
			$("#delStim").prop('disabled',false);
			$("#saveStim").prop('disabled',true);
			console.log("notnew");
		};	
	};
	
	function imgReplaceMsgH(m){
	  console.log(m);
	  $('#hiddenImgBuffer').attr("src",m.src);
	  $('#stimNameShnInput').val(m.IName);
	  $('#imgMetaDataStorage').attr("imgName",m.IName);
	  
	  if(m.isnew){
		$("#imgMetaDataStorage").removeAttr("imgId");
		$("#imgMetaDataStorage").prop("isnew",true).trigger("change"); 
		console.log("Replace with new");
	  }
	  else{
		$('#imgMetaDataStorage').attr("imgId",m.pictId);
		$('#imgMetaDataStorage').prop("isnew", false).trigger('change');
		console.log("Replace with old");		
	} 
	  
	  //$('#imgEditorCanvas').fadeIn();
	};
	
	function clearIOMsgH(m){
	  resetIO();
	};	
	

	toggleContainer = function(e){
			if(e.data.el.css("display")=="none") {
				e.data.itm.slideUp('slow');
				e.data.el.slideDown('slow');
				} 
			else {e.data.el.slideUp('slow')}
		}
		
		
	AOIObjCollection = function(){
	  var stored = [];
	  
	  this.add=function(o){
		  stored.push(o)
	  }
	  
	  this.view=function(){
		  return stored;
	  }
	  
	  this.replace=function(o){
		  stored[o.fabricID]=o;
	  }
	}
	
	
	var OwnCanv = new fabric.Canvas('imgEditorCanvas', { isDrawingMode: false }  );
	
$(document).ready(function(){
  console.log("New value");
  
  fabric.Canvas.prototype.reloadBG = function(imgToSet){
		this.setBackgroundImage(imgToSet.src,this.renderAll.bind(this));
		this.setWidth(imgToSet.width);
		this.setHeight(imgToSet.height);
		
  };
  

  
  var hiddenIn = new Shiny.InputBinding();
  
  

  
  AOIcol = new AOIObjCollection();
  
  
  $.extend(hiddenIn, {
    find: function(scope) {
      return $(scope).find('.shiny-hidden-input');
    },
    getValue: function(el) {
      return $(el).val();
    },
    setValue: function(el, value) {
      $(el).val(value);
    },
    subscribe: function(el, callback) {
      $(el).on('change.hiddenIn', function(event) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off('.hiddenIn');
    },
  });
  

  //addFigure( {c:OwnCanv,figure:MyPolygon})
  $("#aoigroup").val($("input[name='aoigroup']:checked").val());

  $("input[name='aoigroup']")
    .change({p:$("#aoigroup")},function(e){ 
			e.data.p.val($("input[name='aoigroup']:checked").val());
		});
  
  
  Shiny.inputBindings.register(hiddenIn, 'shiny.hiddenIn');
  Shiny.addCustomMessageHandler("replaceImgCallbackHandler",imgReplaceMsgH);
  Shiny.addCustomMessageHandler("resetInputsImgCallbackHandler",clearIOMsgH);

  $("#repStim").prop('disabled',true);
  $("#delStim").prop('disabled',true);
  $("#saveStim").prop('disabled',true);
	
  $('#stimImgShnFileSource').on('change', loadStimFile);
  $('#imgMetaDataStorage').on('change', defineModifyAct);
	
  $("#saveStim").on('click', sendInputData);
  $("#repStim").on('click', sendInputData);
  $("#delStim").on('click', sendInputData);
	
  $("#AddAOI").on('click', {c:OwnCanv,figure:$("#aoigroup")}, addFigure);
	
	
  $('#hiddenImgBuffer').on('load',{cv:OwnCanv},imgUpdate);
  $('#LoadImgContainerClicker').on('click',{el:$('#LoadImgContainer'),itm:$('.clickerItem')},toggleContainer);
  $('#CreateAOIContainerClicker').on('click',{el:$('#CreateAOIContainer'),itm:$('.clickerItem')},toggleContainer);
  
  $('#LoadImgContainer').show();
  $('#CreateAOIContainer').hide();
  
  OwnCanv.on({
  'object:selected': function(e) {
		var current = e.target;
		//console.log(e.target);
		//current.toJSON();
		
  },
   
  'object:modified': function(e) {
		var current = e.target;
		//console.log(e.target);
		//current.toJSON();
		var id = OwnCanv.getObjects().indexOf(current);
		if (id>-1) {
			var o = {	fabricID:	id,
						fabricView:	OwnCanv.toJSON().objects[id],
						name:		OwnCanv.toJSON().objects[id].type};
		
			AOIcol.add(o);
			console.log(AOIcol.view());
		}
		//console.log(current.toJson());
		//console.log(current.DatalessObject())
	}
	
	});
  
});