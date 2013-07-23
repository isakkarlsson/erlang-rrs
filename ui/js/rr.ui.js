$(document).ready(function() {


    var deck = new $.scrolldeck({
        buttons: '.nav-button'
    });
    
    $("input[type=submit], button").button();

    var data = ["Random Forest"];

    $(".method").click(slider("#ml-lip"));
    $("#eval-selector").click(slider("#eval-lip"));

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
