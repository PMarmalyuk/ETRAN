
(function($) {
    $.fn.goTo = function() {
        $('html, body').animate({
            scrollTop: $(this).offset().top + 'px'
        }, 'fast');
        return this; // for chaining...
    }
})(jQuery);

String.prototype.paddingLeft =function(paddingValue){return String(paddingValue + this).slice(-paddingValue.length)};    
String.prototype.replaceAll = function(search, replacement) {
    var target = this;
    return target.split(search).join(replacement);
};

Date.prototype.toDateInputValue = function(short) {
    var local = new Date(this);
    local.setMinutes(this.getMinutes() - this.getTimezoneOffset());
    if(short){
    return local.toJSON().slice(0,10);} else {return (local.toJSON().slice(0,10)+" "+local.toJSON().slice(11,19))}
    }

function coAuthAdd(){
    if(!$(".unset").length==0){
        var p = $($(".unset")[0]);
          p.removeClass( "unset" ).addClass( "set" ); 
          tabRefresh(); 
          tabAction($(".set").length-1,'новый автор');
          $($("a[href='#"+p.attr('id')+"']")[0]).trigger('click');
    }
 }

function showEMsg(eMsg)  {  $("#eMsg").text(eMsg);  $("#eMsg").slideDown("slow").delay(1000).slideUp("slow");};

function getErrorContainer(msg){
  return ('<div class="errorLabel">'+msg+'</div>');
}
function setErrorLabel(element,msg,lifetime){
  $(getErrorContainer(msg)).insertBefore(element.parent()).goTo().slideDown("slow").delay(lifetime).slideUp("slow").promise().done(function() {$(this).remove();});
}
  

function getInfoContainer(msg){
  return ('<div class="infoLabel">'+msg+'</div>');
}
function setInfoLabel(element,msg,lifetime){
  $(getInfoContainer(msg)).insertBefore(element.parent()).goTo().slideDown("slow").delay(lifetime).slideUp("slow").promise().done(function() {$(this).remove();});
}

    //Creating dynamic link that automatically click
    function downloadURI(uri, name) {
        var link = document.createElement("a");
        link.download = name;
        link.href = uri;
		document.body.appendChild(link);
        link.click();
        //after creating link you should delete dynamic link
        //clearDynamicLink(link); 
    }

    //Your modified code.
    function printToFile() {
        div = $("#chordChart")[0];
		html2canvas(div, {
            onrendered: function (canvas) {
                var myImage = canvas.toDataURL("image/png");
                //create your own dialog with warning before saving file
                //beforeDownloadReadMessage();
                //Then download file
                downloadURI("data:" + myImage, "frequencyChart.png");
            }
        });
    }


function reshapeMatrix(m){
	
	function getZeroSeq(n){
	
		return Array.apply(null, Array(n)).map(function () {return 0});
	}
	
	var statesNum = m.legend.length,
		fromState = m.data,
		toState = math.transpose(m.data),
		o = {};
	
    o.Names = m.legend.concat('',m.legend,'');
	o.respondents = fromState.reduce(function(p,c){p+=c.reduce(function(p,c){return p+c},0); return p},0);
	o.emptyPerc = 0.4; //What % of the circle should become empty
	
	emptyStroke = Math.round(o.respondents*o.emptyPerc); 
	var matrix = fromState.reduce(function(p,c){var row = getZeroSeq(statesNum+1).concat(c,0); p.push(row); return p },[]);
	matrix.push(getZeroSeq(2*(statesNum+1)-1).concat(emptyStroke));
	matrix = toState.reduce(function(p,c){var row = c.concat(0,getZeroSeq(statesNum+1)); p.push(row); return p},matrix);
	matrix.push(getZeroSeq(statesNum).concat(emptyStroke,getZeroSeq(statesNum+1)));
	
	o.matrix = matrix;
	o.emptyStroke=emptyStroke;
	return o;
}


frMatrixShow = function(m){	
	doChord(reshapeMatrix(m));
	}

shinySend = function(m){Shiny.shinyapp.sendInput(m)};


waitShinyApp = function(){
		if(typeof window.Shiny.shinyapp !== "undefined"){
			shinySend({waitFrData:"send me AOI data"})
		} else{	setTimeout(waitShinyApp,250)	}
	}
	


$(document).ready(function(){
	
	
	Shiny.addCustomMessageHandler("loadFrMatrixHandler",frMatrixShow);
	waitShinyApp();
	$("#saveChartBtn").on('click',printToFile);
	
})
