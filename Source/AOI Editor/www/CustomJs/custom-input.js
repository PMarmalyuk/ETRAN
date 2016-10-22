$(document).ready(function(){
  //shiny custom input 
  var coordsInputBinding = new Shiny.InputBinding();

  $.extend(coordsInputBinding, {

    find: function(scope) {
      return $(scope).find('.savebtn');
    },

    getValue: function(el) {
      var vl = $(el).val().split('#');

      return vl;
    },

    setValue: function(el, value) {
      $(el).val(value);
    },

    subscribe: function(el, callback) {
      $(el).on('change.coordsInputBinding', function(event) {
        callback();
      });
    },

    unsubscribe: function(el) {
      $(el).off('.coordsInputBinding');
    },

  });

  Shiny.inputBindings.register(coordsInputBinding, 'shiny.coordsInput');
});