$(document).ready(function() {


    var deck = new $.scrolldeck({
        buttons: '.nav-button'
    });
    
    $("input[type=submit], button").button();

    var data = ["Random Forest"];

    $(".method").click(slider("#ml-lip"));
    $("#eval-selector").click(slider("#eval-lip"));

    $(".slider").each(function () {
	$(this).slider({
	    range: "min",
	    value: parseInt($(this).attr("default")),
	    min: parseInt($(this).attr("min")),
	    max: parseInt($(this).attr("max")),
	    slide: function( event, ui ) {
		id = $(this).attr("id") + "-current";
		$("#" + id).text(ui.value);
	    }
	});
    });

    $(".choice").each(function() {
	$(this).chosen({
	    
	});
    });

    $(".help").each(function() {
	var help = $(this);
	if($(help).attr("show") == "true") {
	    $(help).append("<sup>?</sup>");
	}
	var build = $("#build-information");
	$(this).hover(function(a) {
	    var msg = $(help).attr("help");
	    $(build).prepend(""+
"<div class='build-information-bubble'>" +
"  <div class='build-information-bubble-contents'>"+
"   <p> " + msg + "</p> " +
"  </div> " +
"</div>");
	    $("#build-information .build-information-bubble:first").hide().fadeIn("slow");
	}, function (b) {
	    $("#build-information .build-information-bubble:first").delay(1000).fadeOut("slow");
	    
	});
    });

});


function slider(lip) {
    return function () {
	var ml_lip =  $(lip);
	if(ml_lip.is(":visible")) {
	    ml_lip.hide("blind");
	} else {
	    ml_lip.show("blind");
	}
    }

}
