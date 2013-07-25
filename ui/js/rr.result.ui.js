$(document).ready(function() {
    var deck = new $.scrolldeck({
        buttons: '.nav-button'
    });

    $.extend({
	getUrl: function (name) {
	    return decodeURI(
		(RegExp(name + '=' + '(.+?)(&|$)').exec(location.search)||[,null])[1]
	    )
	}
    });
    $.ajax({
	type: "get",
	dataType: "json",
	url: "api/result/get/" + $.getUrl("id"),
	success: function (data) {
	    if(data.type == "cross-validation") {
		handleFolds(data.folds);
	    }
	}});

    function handleFolds(folds) {
	var avg = folds[0].measures;
	for(key in avg) {
	    $("#statistics .attributes").append(
		"<tr>" +
		"  <td class='attr-key'>" + key+ "</td>" +
		"  <td class='attr-value'>" + avg[key] + "</td>" + 		
		"</tr>");
	}
    }
});
