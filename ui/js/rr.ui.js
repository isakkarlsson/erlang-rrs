$(document).ready(function() {
<<<<<<< HEAD
    var block = function (id) {
	$(id).block({
	    message: "<h1>You must complete the previous step first!</h1>",
	    css: {
		cursor: 'default'
	    } 
	});
    };
=======
>>>>>>> 00170abde6bc33bb4848387df4b0964ce552ad06
    block("#eval .main, #build .main");
    $("#progress-bar").progressbar({value:30});
    // $("#top-panel").block({
    // 	message: $("#progress-dialog")
    // });
    var deck = new $.scrolldeck({
        buttons: '.nav-button'
    });
    $("input[type=submit], button").button();
    var resetMlLip = function () {
	$("#ml-lip").hide("blind");
	$("#ml-lip table.attributes").html("");
	block("#eval .main");
    };

    var resetEvalLip = function() {
	$("#eval-lip").hide("blind");
	$("#eval-lip table.attributes").html("");
    }

    loadFiles = function () {
	$.ajax({
	    type: "get",
	    dataType: "json",
	    url: "api/dataset/files",
	    success: function (data) {
		var files = $("#files select.choice");
		for(file_id in data) {
		    file = data[file_id];
		    files.append("<option value='" + file_id + "'>" + 
				 file.name + " ("  + file.no_features + "/" + file.no_examples + ")" +
				 "</option>");
		}
		$("#files .choice").chosen({
		    width: "100%"
		}).change(function () {
		    var file_id = $(this).attr("value");
		    block("#build .main, #eval .main");
		    $("#files-confirm")
			.switchClass("gray", "green")
			.click(function(e) {
			    e.preventDefault();
			    $("#build .main").unblock();
			    resetMlLip();
			    resetEvalLip();
			    loadLearners(data[file_id]);
			    deck.toId("#build");
			});
		});
	    }
	});
    }
    loadFiles();

    loadLearners = function(dataset) {
	// load machine learning algorithms from server using /ml/get-all
	$.ajax({
	    type: "get",
	    dataType: "json",
	    url: "api/machine-learning/get-all",
	    success: function (data) {
		loader({
		    current: {
			buttons: ".machine-learner",
			lip: "#ml-lip",
			menu: "#ml-selector",
			confirm: "#ml-confirm",
			back: "#ml-back",
			data: data,
			valueHandler: new ValueHandler(dataset),
			reset: resetMlLip		
		    },
		    next: {
			load: function() {
			    $("#eval .main").unblock();
			    var learner = collect({
				menu: "#ml-selector",
				lip: "#ml-lip"
			    });
			    loadEvaluators(learner, dataset);
			    deck.toId("#eval");
			}
		    },
		    prev: {
			load: function() {
			    deck.toId("#dataset");
			}
		    }
		});
	    }});
    };

    loadEvaluators = function(learner, dataset) {
		// load machine learning algorithms from server using /ml/get-all
	$.ajax({
	    type: "get",
	    dataType: "json",
	    url: "api/evaluator/get-all",
	    success: function (data) {
		loader({
		    current: {
			buttons: ".evaluator",
			lip: "#eval-lip",
			menu: "#eval-selector",
			confirm: "#eval-confirm",
			back: "#eval-back",
			data: data,
			valueHandler: new ValueHandler(dataset),
			reset: resetEvalLip
		    },
		    next: {
			load: function() {
			    var evaluator = collect({
				menu: "#eval-selector",
				lip: "#eval-lip"
			    });
			    var payload = {
				evaluator: evaluator,
				learner: learner,
				file: dataset
			    };

			    runModel(payload);
			}
		    },
		    prev: {
			load: function() {
			    deck.toId("#build");
			}
		    }
		});
	    }});
	
    };
  

    $(".help").live({
	mouseenter: function() {
	    var build = $("#build-information");
	    var msg = $(this).attr("help");
	    $(build).prepend(""+
			     "<div class='build-information-bubble'>" +
			     "  <div class='build-information-bubble-contents'>"+
			     "   <p> " + msg + "</p> " +
			     "  </div> " +
			     "</div>");
	    $("#build-information .build-information-bubble:first").fadeIn("slow");
	},
	mouseleave: function (b) {
	    $("#build-information .build-information-bubble:first").delay(1000).fadeOut("slow");
	}
    });

    function loader (props) {
	var list = $(props.current.menu + " ul");
	list.html("");
	for(algorithm_key in props.current.data) {
	    alg = props.current.data[algorithm_key];
	    list.append(
		"<li><a href='#'" +
		    "       class='" + props.current.buttons.replace(".", "") + " method help'" + 
		    "       key='" + algorithm_key + "'" +
		    "       help='" + alg.help + "'>" + alg.name + "</a>" +
		    "</li>");
	}

	$(props.current.buttons).click(function(e) {
	    e.preventDefault();
	    if($(this).hasClass("active")) {
		return;
	    }


	    props.current.reset();
	    $(props.current.menu + " .active").removeClass("active");
	    $(this).addClass("active");

	    var key = $(this).attr("key");
	    var options = props.current.data[key].options;
	    for(option in options) {
		var value = options[option];
		var item =
 		    "<tr class='attribute'>" + 
		    "  <td class='attr-key'>" +
		    "    <span class='help' key='" + option + "' help='" + value.help + "' show='true'>" + value.name + "</span>" +
		    "  </td>"+
		    "  <td class='attr-value'>";

		if(value.type == "numeric") {
		    item += "<div key='" + option + "' class='slider' min='" + 
			value.min + "' max='" + 
			props.current.valueHandler.handleMax(value) + "' default='" + 
			props.current.valueHandler.handleDefault(value) + "' step='" + 
			props.current.valueHandler.handleStep(value) + "'></div>";
		    item += "<td class='attr-progress'><span id='" + option + "-current'>" + 
			props.current.valueHandler.handleDefault(value) + "</span></td>";
		    item += "</td>";
		} else if (value.type == "categoric") {
		    item += "<select key='" + option + "'class='choice'>";
		    for(op in value.options) {
			opt = value.options[op];
			item += "<option value='" + op + "'>" + opt.name + "</option>";
		    }
		    item +="</select>"
		    item +="</td><td class='attr-progress'></td>";
		    
		}
		item += "</tr>";
		$(props.current.lip + " table.attributes").append(item);
	    }

	    initSlider(props.current.lip);
	    initChosen(props.current.lip);
	    $(props.current.lip).show("blind");

	    $(props.current.confirm).switchClass("gray", "green").click(function(e) {
		e.preventDefault();
		props.next.load();
	    });

	    $(props.current.back).click(function(e) {
		e.preventDefault();
		props.back.load();
	    });
	});
    }

    
});


function collect(props) {
    var id = $(props.menu + " .active").attr("key");
    var map = {id: id};
    $(props.lip + " .attr-value").each(function() {
	var value = $(this).children().first();
	if($(value).hasClass("slider")) {
	    map[$(value).attr("key")] = $(value).slider("value");
	} else if ($(value).hasClass("choice")) {
	    map[$(value).attr("key")] = $(value).val();
	}
    });
    return map;
};

function ValueHandler (dataset) {
    this.dataset = dataset;    
    this.handleMax = function (opt) {
	if(opt.max == "no_features") {
	    return this.dataset.no_features;
	} else if (opt.max == "no_examples") {
	    return this.dataset.no_examples;
	}
	
	return opt.max;
    }

    this.handleDefault = function(opt) {
	if(opt.default == undefined) {
	    return opt.min;
	} else if (opt.default == "log") {
	    return Math.round(Math.log(this.dataset.no_features)/Math.log(2) + 1);
	}

	return opt.default;
    }

    this.handleStep = function(opt) {
	if(opt.step == undefined) {
	    return 1;
	} else {
	    return opt.step;
	}
    }
}



function initChosen(parent) {
    $(parent + " .choice").each(function() {
	$(this).chosen({
	    width: '98%'
	});
    });
}

function initSlider(parent) {
    $(parent+" .slider").each(function () {
	$(this).slider({
	    range: "min",
	    value: parseInt($(this).attr("default")),
	    min: parseInt($(this).attr("min")),
	    max: parseInt($(this).attr("max")),
	    step: parseInt($(this).attr("step")),
	    slide: function( event, ui ) {
		id = $(this).attr("key") + "-current";
		$("#" + id).text(ui.value);
	    }
	});
    });
}


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

function runModel(payload) {
    alert($.toJSON(payload));
    block("#dataset .main, #build .main", 
	  "<h1>Working! Please wait...</h1>");
}

function block(id, message) {
    message = message || "<h1>You must complete the previous step first!</h1>";
    $(id).block({
	message: message,
	css: {
	    cursor: 'default'
	} 
    });
};

