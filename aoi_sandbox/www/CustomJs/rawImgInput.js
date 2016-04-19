$(document).ready(function(){
 
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
  Shiny.inputBindings.register(hiddenIn, 'shiny.hiddenIn');
  
  Shiny.addCustomMessageHandler("replaceImgCallbackHandler", function(m){
	  console.log(m);
	  $('#imgMetaDataStorage').attr("imgName",m.IName);
	  $('#imgMetaDataStorage').attr("imgId",m.pictId);
	  $('#imgMetaDataStorage').prop("isnew", false).trigger('change');
	  $('#hiddenImgBuffer').attr("src",m.src);
  })
	
  
  Shiny.addCustomMessageHandler("resetInputsImgCallbackHandler", function(m){
	  resetIO();
  })

	$("#repStim").prop('disabled',true);
	$("#delStim").prop('disabled',true);
	$("#saveStim").prop('disabled',true);
	
	
	
  	$('#stimImgShnFileSource').on('change', loadStimFile);
	$('#imgMetaDataStorage').on('change', defineModifyAct);
	
	$("#saveStim").on('click', sendInputData);
	$("#repStim").on('click', sendInputData);
	$("#delStim").on('click', sendInputData);
	
	$('#hiddenImgBuffer').on('load',imgUpdate);
	
	
	
  
});