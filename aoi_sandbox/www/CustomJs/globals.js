	var OwnCanv;
	var MLTlst;
	var AOIListContent;
	
	var renameAOIBTN;
	var AOI_SetName;
	var AOINameSelectBox;
	var userCanvas;
	var bufferCanvas;
	var AOIName;
	var AOIGroup;
	var stimWidth;
	var stimHeight;
	var stimName;
	var stimFileSrc;
	var stimFileSrcProgress;
	
	var stimRepBTN;
	var stimDelBTN;
	var stimSaveBTN;
	
	var stimulEditorPanel;
	var stimulEditor;
	
	var AOIEditorPanel;
	var AOIEditor;
	
	var AOIsetEditorPanel;
	var AOIsetEditor;
	
	var addAOIBTN;
	var stimulMetaData;
	
	var AOIparams;
	var repSelectedSet;
	var drawMe=false;
	
	shinySend = function(m){Shiny.shinyapp.sendInput(m)};
	labelMsg = function(label,Obj){console.log(label); console.log(Obj)};
	
	Array.prototype.last = function(){return this[this.length - 1]};


$(document).ready(function(){
	
	
	 AOINameSelectBox = $('#AOINameInput');
	 userCanvas = $('#imgEditorCanvas');
	 bufferCanvas = $('#hiddenImgBuffer');
	 
	AOIparams= $('#snglAOIparams');
	

	
	 AOIName = $('#AOINameInput');
	 AOIGroup = $('#aoigroup');
	 stimWidth = $('#stimWidthShnInput');
	 stimHeight = $('#stimHeightShnInput');
	 stimName = $('#stimNameShnInput');
	 stimFileSrc = $('#stimImgShnFileSource');
	 stimFileSrcProgress = $('#stimImgShnFileSource_progress');
	
	stimRepBTN = $('#repStim');
	 stimDelBTN = $('#delStim');
	 stimSaveBTN = $('#saveStim');
	 
	 renameAOIBTN= $('#RenameAOI')
	 
	 
	
	
	stimulEditorPanel = $('#LoadImgContainerClicker');
	stimulEditor = $('#LoadImgContainer');
	
	AOIEditorPanel = $('#CreateAOIContainerClicker');
	AOIEditor = $('#CreateAOIContainer');
	
	AOIsetEditorPanel = $('#CreateSetContainerClicker');
	AOIsetEditor = $('#CreateSetContainer');
	
	addAOIBTN = $('#AddAOI');
	
	stimulMetaData = $('#imgMetaDataStorage');
	
	AOIListContent = $('#SelectContainer');	
	AOI_SetName=$('#AOISetNameInput');
	
	
	MLTlst = $('.AOImltS').bootstrapDualListbox({
		nonSelectedListLabel: 'List of existing AOIs',
		selectedListLabel: 'Selected areas',
		preserveSelectionOnMove: 'moved',
		showFilterInputs: false,
		infoText: false,
		moveOnSelect: false
	});
	
	$("div.box2").find("div.btn-group.buttons").append('<button type="button" text="set" id="SaveSelection" style="width: 20%" class="btn btn-default">new set</button>');
	$("div.box2").find("div.btn-group.buttons").append('<input type="text" id="newSetFromList" style="width: 100%" placeholder="new set name" />');
	$("div.box1").find("div.btn-group.buttons").append('<input type="button" name="Hi" id="hideAll" style="width: 20%" class="btn btn-default"/>');

	saveSelectedAsSet= $('#SaveSelection')
	newSetFromList=$('#newSetFromList')
	repSelectedSet=$('#repSet');
	delSelectedSet=$('#delSet');	

	
})
	