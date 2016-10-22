  //shiny custom input 
  var coordsInputBinding = new Shiny.InputBinding();

  $.extend(coordsInputBinding, {

    find: function(scope) {
      return $(scope).find('.savebtn');
    },

    getValue: function(el) {
      //вернуть значение
      var ans = '';
        var objs = OwnCanv.getObjects();
        var json_data = JSON.stringify(objs[1]);
        var json_data2 = JSON.parse(json_data);
        var new_coords;
        if ($('.success').length == 1)
        {
          var id_cur = $('.success').find('.id').html();
          var type_cur = $('.success').find('.coords-type').html();

          if (parseInt(type_cur) == 3)
          {
            var x1 = parseInt(json_data2['left']);
            var y1 = parseInt(json_data2['top']);
            var scX = parseInt(json_data2['scaleX']);
            var scY = parseInt(json_data2['scaleY']);
            var w = parseInt(json_data2['width']);
            var h = parseInt(json_data2['height']);
            var x2 = w*scX+x1;
            var y2 = h*scY+y1;
            new_coords = x1+','+y1+','+x2+','+y2
          }

          var trs = $('#history').find('tr');

          var trs = $('#history').find('tr');
      var row;
      var id2;
      var tds;
      for (var j = 0; j < trs.length; j++)
      {
        tds = $(trs[j]).find('td');
        row = parseInt($(tds[0]).html());
        id2 = parseInt($(tds[1]).html());
        if (id_cur == id2) 
        {
          ans = id_cur+','+new_coords+','+type_cur+',0,0,0';
          break;
        }
      }
      var new_id = id2+1
      if (ans == '') ans = new_id+','+new_coords+','+type_cur+',0,0,0';
          
        }
      return ans;
    },

    setValue: function(el, value) {
      el.value = value;
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