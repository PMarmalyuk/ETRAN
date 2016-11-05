	waitShinyApp = function(){
		if(typeof window.Shiny.shinyapp !== "undefined"){
			shinySend({waitForAOI:"send me AOI data"})
		} else{	setTimeout(waitShinyApp,250)	}
	}
	
	plotByCollectionElement = function(e){
		var commonParam={fill: 'red', opacity: 0.5 , fill: 'red', opacity: 0.5, selectable: true}; 
		if (e.rType=="Polyhedron"){
			var figParams = e.rData.map(function(element){return {x:element[0],y:element[1]}})
			var figToAdd = new fabric.Polygon(figParams);
		}
		if(e.rType=="Ellipse"){
			var figParams = {rx:e.rData.MajAxis, ry:e.rData.MinAxis, angle:e.rData.Angle,top:e.rData.CY ,left:e.rData.CX, originX:"center", originY:"center"}
			var figToAdd = new fabric.Ellipse(figParams);
		}	
		figToAdd.set(commonParam); figToAdd.hide();	OwnCanv.add(figToAdd);
	}

	reloadAOIinfo = function(e) {
			var current = e.target;
			var id = OwnCanv.getObjects().indexOf(current);
			if (id>-1) {
				var s=AOIcol.getObject(id);
				var o = {	name:		s.name,
							rId:		s.rId,
							rType:		s.rType,
							rData:		current.getRDisp()};
				AOIcol.replace(o,id);
				console.log(AOIcol.view());
			}
		}
		
	reloadMultipleAOIinfo=function() {
			var objects = OwnCanv.getObjects();
			objects.forEach(function(element,index,array){
				var s=AOIcol.getObject(index);
				var o = {	name:		s.name,
							rId:		s.rId,
							rType:		s.rType,
							rData:		element.getRDisp()
							};
				AOIcol.replace(o,index);
			})
			AOIcol.current=-1;
			AOIparams.ds();
		}
		
		
	setEditActive=function(e) {
		var current = e.target;
		var id = OwnCanv.getObjects().indexOf(current);
		if(id>-1){
			AOINameSelectBox.val(AOIcol.getObject(id).name);
			 AOIcol.current=id;
			 AOIparams.en();
			 //var sit=AOIListContent.find("#bootstrap-duallistbox-nonselected-list_duallistbox").children()[id];
			 //$(sit).prop("selected",true)
			 //$("button.move").trigger("click");
		} else {/*AOIparams.ds();*/ AOIcol.current=-1;};
	}
	
		
		
	AOIObjCollection = function(){
	  var stored = [];
	  this.nextId = 1;
	  
	  this.load =function(o){stored.push(o);	$(this).trigger('change')}
	  this.view=function(){return stored}
	  this.getObject=function(i){return stored[i]}
	  this.getIdByUID=function(uid){
		  return stored.reduce(function(p,c,i){if(c.rId==uid){p=i}  return(p)},-1)
		  }
	  
	  this.forceToCanv=function(){stored.forEach(function(element){plotByCollectionElement(element)})}
	  
	  this.add=function(o){
		o.rId=this.nextId; this.nextId++;  stored.push(o);
		var AOIObj={name:o.name,	shape:o.rType,	data:o.rData,	id:o.rId};
		shinySend({newAOIObj: AOIObj});
		$(this).trigger('change');
	  }
	  
	  this.replace=function(o,i){
		stored[i]=o;
		var AOIObj={name:o.name,	shape:o.rType,	data:o.rData,	id:o.rId};
		shinySend({repAOIObj: AOIObj});
		//$(this).trigger('change');
	  }
	  
	  this.del=function(i){
		shinySend({delAOIObj: stored[i].rId});
		stored.splice(i,1);
		this.current=-1;
		$(this).trigger('change');
	  } 
	  
	  this.getCurrent=function(){if (this.current>-1) {return stored[this.current]} else return -1}
	  
	  this.current=-1;
	}
	
	
	
	onOptionClick=function(e){
		AOINameSelectBox.val(AOIcol.getObject(e.data.id).name)
		
		var a=AOIListContent.find("#bootstrap-duallistbox-nonselected-list_duallistbox").children();
		a.each(function(i,e){
			OwnCanv.getObjects()[parseInt($(e).val())].hide();
		});
		
		canvObj=OwnCanv.getObjects()[e.data.id];
		canvObj.show();
		OwnCanv.setActiveObject(canvObj);
		
		OwnCanv.renderAll;
		}
	
	onKeyDownHandler=function(e) {
		switch (e.keyCode) {
		case 46: // delete
			var activeObject = OwnCanv.getActiveObject();
			if (activeObject) {
				AOIcol.del(OwnCanv.getObjects().indexOf(activeObject));			
				OwnCanv.remove(activeObject)};
				
				if (OwnCanv.getObjects().last()!=null) {
					OwnCanv.setActiveObject(OwnCanv.getObjects().last())} 
				else {AOIparams.ds();} ;
				
				OwnCanv.renderAll();
			return;
			
		case 32://104: // "h"
		/*
			var activeObject = OwnCanv.getActiveObject();
			if (activeObject) {activeObject.toggleVisability()}
			OwnCanv.renderAll();*/
			return;
		}
	};

	
updateMultipleSelection = function(){
	
		//var a=$("#div.bootstrap-duallistbox-container.row").find("option");
		//sel_items=a.map(function(element){return($(element).prop(selected))});
		
		MLTlst.empty();
		AOIcol.view().forEach(function(element, index){
			MLTlst.append('<option value="'+index+'">'+element.name+'</option>');
			$(MLTlst.children()[index]).on('click',{id:index},onOptionClick);
		})
		MLTlst.bootstrapDualListbox('refresh');
		
		//a=AOIListContent.find("#bootstrap-duallistbox-nonselected-list_duallistbox").children()
		
		//sel_items.map(function(element,index){if(element){$(a[index]).prop('selected',true)}})
		//$("button.move").trigger("click");
	}
	
loadAOICollectionMsgH = function(m){
		if (m!=undefined){
			m.data.forEach(function(element){
				var o = {rType:element.rType, rData:element.rData, name:element.name, rId:element.rId}
				AOIcol.load(o);
			})
		AOIcol.nextId=m.nextId; AOIcol.forceToCanv();
		}
	}
	

setNewAOIName = function(e){
	o=AOIcol.getCurrent();
	ind=AOIcol.view().indexOf(o);
	if (o!=-1) {o.name=AOIName.val();AOIcol.replace(o,AOIcol.current)}
	
	$(MLTlst.children()[ind]).html(AOIName.val())
	MLTlst.bootstrapDualListbox('refresh');
	}
	
$(document).ready(function(){
	AOIcol = new AOIObjCollection();
	

	

OwnCanv.on({
		'object:selected': setEditActive,
		'object:modified': reloadAOIinfo,
		'selection:cleared':reloadMultipleAOIinfo
	});
	

	
	$(AOIcol).on('change',updateMultipleSelection);
	$(window).on('keydown', onKeyDownHandler);
	
	renameAOIBTN.on('click',setNewAOIName);
  
  	Shiny.addCustomMessageHandler("loadAOICollectionCallbackHandler",loadAOICollectionMsgH);
	
	
	waitShinyApp();

})

