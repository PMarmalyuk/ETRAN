function waitShinyApp(){
    if(typeof window.Shiny.shinyapp !== "undefined"){
        console.log("shinyapp found!");
		requestAOIData();
    }
    else{
        setTimeout(waitShinyApp,250);
    }
}
	
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
		OwnCanv.reloadBG(e.target);
		fillInputData(false);
		$(e.target).hide();
	};
		
	function addFigure(e){
		var fig = e.data.figure.val();
		var commonParam={fill: 'red', opacity: 0.5 , left: 50, top: 50, fill: 'red', opacity: 0.5, selectable: true};
		
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
		}
		
		figToAdd.set(commonParam);
		OwnCanv.add(figToAdd);//.setActiveObject(figToAdd);
		
		var id = OwnCanv.getObjects().indexOf(figToAdd);
		if (id>-1) {
			if ($('#AOINameInput').val()==''){$('#AOINameInput').val(OwnCanv.toJSON().objects[id].type)};
			var o = {	fabricID:	id,
						fabricView:	OwnCanv.toJSON().objects[id],
						name:		$('#AOINameInput').val(),
						rType:		rType,
						rData:		rData
						};
		
			AOIcol.add(o);
			
			//AOIListContent.append('<option value="'+o.fabricID+'">'+o.name+'</option>');
			//AOIListContent.bootstrapDualListbox('refresh');
		}
		
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
	
	
	function requestAOIData(){
		var waitForAOI={waitForAOI:"send me AOI data"};
		console.log(waitForAOI);
		Shiny.shinyapp.sendInput(waitForAOI);
	}
	
	function loadAOICollectionMsgH(m){
		//console.log(m);
		if (m!=undefined){
			m.data.forEach(function(element){
				var o = {
						rType : element.rType,
						rData : element.rData,
						name	: element.name,
						rId		: element.rId
				}
				AOIcol.load(o);
			})
			AOIcol.nextId=m.nextId;
			AOIcol.forceToCanv();
		}
	}
	
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
				if(e.data.isAOI) {AOIListContent.fadeIn()} else {AOIListContent.fadeOut()};
				} 
			else {e.data.el.slideUp('slow')}
		}
				
	reloadAOIinfo=function(e) {
		var current = e.target;
		var id = OwnCanv.getObjects().indexOf(current);
		if (id>-1) {
			var s=AOIcol.getObject(id);
			var o = {	fabricID:	id,
						fabricView:	OwnCanv.toJSON().objects[id],
						name:		s.name,
						rId:		s.rId,
						rType:		s.rType,
						rData:		current.getRDisp()};
			AOIcol.replace(o);
			console.log(AOIcol.view());
		}
	}
	
	setEditActive=function(e) {
		var current = e.target;
		var id = OwnCanv.getObjects().indexOf(current);
		if(id>-1){
			$('#AOINameInput').val(AOIcol.getObject(id).name)
		}
	}
	
	reloadAOIinfoF=function(e) {
		var current = e.target;
		var objects = OwnCanv.getObjects();
		objects.forEach(function(element,index,array){
			var s=AOIcol.getObject(index);
			var o = {	fabricID:	index,
						fabricView:	element.toJSON(),
						name:		s.name,
						rId:		s.rId,
						rType:		s.rType,
						rData:		element.getRDisp()
						};
			AOIcol.replace(o);
		})
		console.log(AOIcol.view());
	}
	
		
		
	AOIObjCollection = function(){
	  var stored = [];
	  this.nextId = 0;
	  this.load =function(o){
		  stored.push(o);
		  $(this).trigger('change');
	  }
	  
	  this.clear = function () {stored=[]}
	  
	  this.add=function(o){
		o.rId=this.nextId;
		stored.push(o);
		$(this).trigger('change');
		  
			var AOIObj={	name:	o.name,
							shape:	o.rType,
							data:	o.rData,
							id:		o.rId
						};
			
			Shiny.shinyapp.sendInput({newAOIObj: AOIObj});
			this.nextId++;
	  }
	  
	  this.view=function(){
		  return stored;
	  }
	  
	  this.replace=function(o){
		  stored[o.fabricID]=o;
		  	var AOIObj={	name:	o.name,
							shape:	o.rType,
							data:	o.rData,
							id:		o.rId
						};
			Shiny.shinyapp.sendInput({repAOIObj: AOIObj});
	  }
	  
	  this.del=function(i){
		  Shiny.shinyapp.sendInput({delAOIObj: this.view()[i].rId});
		  stored.forEach(function(element,index){
			  if (index>i){
				  element.fabricID-=1;
			  }
			});
		  stored.splice(i,1);
		  $(this).trigger('change');
	  }
	  
	  this.getObject=function(i){return stored[i]}
	  
	  this.forceToCanv=function(){
		var commonParam={fill: 'red', opacity: 0.5 , fill: 'red', opacity: 0.5, selectable: true};  
		stored.forEach(function(element){
			if (element.rType=="Polyhedron"){
				var figParams = element.rData.Vertexes.map(function(element){return {x:element[0],y:element[1]}})
				var figToAdd = new fabric.Polygon(figParams);
			}
			
			if(element.rType=="Ellipse"){
				/////
			}	
			figToAdd.set(commonParam);
			OwnCanv.add(figToAdd);			
		})
		
	  }
	    
	}
	
	fabric.Canvas.prototype.reloadBG = function(imgToSet){
		this.setBackgroundImage(imgToSet.src,this.renderAll.bind(this));
		this.setWidth(imgToSet.width);
		this.setHeight(imgToSet.height);
	};
	
	fabric.Object.prototype.hide = function(){this.set({opacity:0,selectable:false,hidden:true,active:false})}
	fabric.Object.prototype.show = function(){this.set({opacity:0.5,selectable:true,hidden:false,active:true})}
	fabric.Object.prototype.toggleVisability = function(){if(this.hidden){this.show()}else{this.hide()}}
	fabric.Object.prototype.hidden = false;
	
	fabric.Polygon.prototype.GetGlobalPNTS = function() {
		 /*создаём точку с координатами [0,0], она константна, далее используем 
		 её как один из параметров метода определённого в fabric.util, вероятно есть 
		 более симпатичный способ принятый в js но разбираться лень*/
		const ZPoint = new fabric.Point(0,0);
		
		/*сюда мы будем записывать координаты вершин вычисленные относительно origin канваса,
		у нас это будет всегда верхний левый угол
		*/
		var MyPoints=new Array(0);

		/*для всех точек лежащих в свойстве points данного многоугольника пересчитываем координаты*/
		for (var i = 0, len = this.points.length; i < len; i++) {
			/*создаём временную переменную для записи новых координат i-й точки*/
			var MyPoint=new fabric.Point;
			/*пишем в эту переменную координаты i-й точки многоугольника*/
			MyPoint.setFromPoint(this.points[i]);
			/*считаем для нашей точки новые координаты исходя из scale данного многоугольника*/
			MyPoint.x*=this.scaleX;
			MyPoint.y*=this.scaleY;
			/*считаем для нашей точки новые координаты исходя из угла на который повернут многоугольник
			фактически мы поворачиваем эту точку на заданный угол относительно центра вращения всей фигуры
			т.к. все координаты вершин заданы относительно origin многоугольника, координаты центра его
			вращения в его системе координат-[0,0]. Это утверждение справедливо когда центр вращения совпадает
			с origin фигуры.
			*/
			MyPoint=fabric.util.rotatePoint(MyPoint,ZPoint,fabric.util.degreesToRadians(this.angle));
			/*Теперь мы знаем точные координаты точки относительно центра фигуры, после применения масштабирования и вращения. Чтобы
			посчитать абсолютную позицию (относительно origin канваса), нужно прибавить к координатам к коорднатам MyPoint
			прибавить соответствующие координаты центра фигуры вычисленные относительно канваса. Делаем это методом addEquals.
			*/
			MyPoint.addEquals(this.getCenterPoint());
			/*Пишем новую точку в массив и повтолряем всё для следующей точки*/
			MyPoints.push(MyPoint);
		}
		return MyPoints;
	  }
	fabric.Polygon.prototype.getRDisp = function(){return {Vertexes:this.GetGlobalPNTS().map(function(element){return [element.x,element.y]})}};
	
	onOptionClick=function(e){
		$('#AOINameInput').val(AOIcol.getObject(e.data.id).name)
		OwnCanv.setActiveObject(OwnCanv.getObjects()[e.data.id]);
		}
	
	onKeyDownHandler=function(e) {
		switch (e.keyCode) {
		case 46: // delete
			var activeObject = OwnCanv.getActiveObject();
			if (activeObject) {
				AOIcol.del(OwnCanv.getObjects().indexOf(activeObject));			
				OwnCanv.remove(activeObject)};
				console.log(OwnCanv.getObjects());
				console.log(AOIcol.view());
			return;
			
		case 32://104: // "h"
			var activeObject = OwnCanv.getActiveObject();
			if (activeObject) {activeObject.toggleVisability()};
			OwnCanv.renderAll;
				console.log(OwnCanv.getObjects());
				console.log(AOIcol.view());
			return;
		}
	};
	
	
	toggleCanvasObj = function(e){
		OwnCanv.getObjects()[e.target.id].visibility(false);
	}
	
	
	updateMultipleSelection=function(){
		MLTlst.empty();
		AOIcol.view().forEach(function(element, index){
			MLTlst.append('<option value="'+element.fabricID+'">'+element.name+'</option>');
			$(MLTlst.children()[index]).on('click',{id:index},onOptionClick);
		})
		MLTlst.bootstrapDualListbox('refresh');
	}

	var OwnCanv;
	var MLTlst;
	var AOIListContent;
	
	
	
$(document).ready(function(){
    
  OwnCanv = new fabric.Canvas('imgEditorCanvas', { isDrawingMode: false }  );
  OwnCanv.on({
  'object:selected': setEditActive,
  'object:modified': reloadAOIinfo,
  'selection:cleared':reloadAOIinfoF
  });
  

  
  var hiddenIn = new Shiny.InputBinding();
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


  AOIListContent = $("#SelectContainer");	
  AOIListContent.hide();
  
  MLTlst = $('.AOImltS').bootstrapDualListbox({
	nonSelectedListLabel: 'List of existing AOIs',
	selectedListLabel: 'Selected areas',
	preserveSelectionOnMove: 'moved',
	showFilterInputs: false,
	infoText: false,
	moveOnSelect: false
	});
	

  $("#aoigroup").val($("input[name='aoigroup']:checked").val());
  $("input[name='aoigroup']")
    .change({p:$("#aoigroup")},function(e){ 
			e.data.p.val($("input[name='aoigroup']:checked").val());
		});
  

  
  Shiny.inputBindings.register(hiddenIn, 'shiny.hiddenIn');
  Shiny.addCustomMessageHandler("replaceImgCallbackHandler",imgReplaceMsgH);
  Shiny.addCustomMessageHandler("resetInputsImgCallbackHandler",clearIOMsgH);
  Shiny.addCustomMessageHandler("loadAOICollectionCallbackHandler",loadAOICollectionMsgH);


  $("#repStim").prop('disabled',true);
  $("#delStim").prop('disabled',true);
  $("#saveStim").prop('disabled',true);
	
  $('#stimImgShnFileSource').on('change', loadStimFile);
  $('#imgMetaDataStorage').on('change', defineModifyAct);
	
  $("#saveStim").on('click', sendInputData);
  $("#repStim").on('click', sendInputData);
  $("#delStim").on('click', sendInputData);
	
  $("#AddAOI").on('click', {figure:$("#aoigroup")}, addFigure);
  
	
  $('#hiddenImgBuffer').on('load',imgUpdate);
  $('#LoadImgContainerClicker').on('click',{el:$('#LoadImgContainer'),itm:$('.clickerItem')},toggleContainer);
  $('#CreateAOIContainerClicker').on('click',{el:$('#CreateAOIContainer'),itm:$('.clickerItem'),isAOI:true},toggleContainer);
  $('#CreateSetContainerClicker').on('click',{el:$('#CreateSetContainer'),itm:$('.clickerItem'),isAOI:true},toggleContainer);
  
  $('#LoadImgContainer').show();
  $('#CreateAOIContainer').hide();
  $('#CreateSetContainer').hide();
  
  AOIcol = new AOIObjCollection();

  $(AOIcol).on('change',updateMultipleSelection);
  //$(AOIcol).on('create',requestAOIData);
  
  window.onkeydown = onKeyDownHandler;
  
	waitShinyApp();

});