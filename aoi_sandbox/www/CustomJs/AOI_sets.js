delSet = function(){	
	var fSetData={		delSet:						"cDelSet",
						uid:						parseInt($('#AOIsetList').val())};
						
	shinySend({delSetMs:fSetData});
}

repSet = function(){
	AOI_SetName.text(newSetFromList.val())
	newSetFromList.val("")
	var a=AOIListContent.find("#bootstrap-duallistbox-selected-list_duallistbox").children();
	var t_set=new Array();
	
	
	a.each(function(i,e){
		t_set.push(AOIcol.view()[parseInt($(e).val())].rId);
		});
		AOI_SetName
	
	var fSetData={		repSet:						"cRepSet",
						setData:					t_set,
						name:						AOI_SetName.val(),
						uid:						parseInt($('#AOIsetList').val())};
						
	shinySend({repSetMs:fSetData});
}

createSet=function(e) {
	
	AOI_SetName.val(newSetFromList.val())
	newSetFromList.val("")
	var a=AOIListContent.find("#bootstrap-duallistbox-selected-list_duallistbox").children();
	var t_set=new Array();
	
	
	a.each(function(i,e){
		t_set.push(AOIcol.view()[parseInt($(e).val())].rId);
		});
		
	var fSetData={		newSet:						"cNewSet",
						setData:					t_set,
						name:						AOI_SetName.val()};
						
	shinySend({newSet:fSetData});
	
	$('.clickerItem').slideUp('slow');
	AOIsetEditor.slideDown('slow');
	
	console.log(t_set);
	return(t_set);
}


aoisetReplaceMsgH =function(m){
	var setIds=m.ids;
	var a=AOIListContent.find("#bootstrap-duallistbox-selected-list_duallistbox").children();
	a.each(function(i,e){
		$(e).prop("selected",true);
		});
	
	$("button.remove").trigger("click");
	a=AOIListContent.find("#bootstrap-duallistbox-nonselected-list_duallistbox").children();
	
	//setIds.map(function(element){a[AOIcol.getIdByUID(element)]})
	
	a.each(function(i,e){
			var cV=setIds.indexOf(parseInt($(e).val())+1);
			if (cV>=0){
				$(e).prop("selected",true)
				OwnCanv.getObjects()[i].show()
				} else 
				{OwnCanv.getObjects()[i].hide()};
		});
	OwnCanv.renderAll();
	$("button.move").trigger("click");
	AOI_SetName.val(m.name)
	
	//MLTlst.remove();
}
	
clearAOIMsgH = function(m){
	 console.log(m);
	}

$(document).ready(function(){
	
saveSelectedAsSet.on('click', createSet);
repSelectedSet.on('click', repSet);
delSelectedSet.on('click', delSet);

Shiny.addCustomMessageHandler("replaceAOI_setCallbackHandler",aoisetReplaceMsgH);
Shiny.addCustomMessageHandler("resetInputsAOICallbackHandler",clearAOIMsgH);

	OwnCanv.getObjects().forEach(function(element){element.hide()})
	OwnCanv.renderAll();

})