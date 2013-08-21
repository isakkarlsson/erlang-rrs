$(document).ready(function() {
    $.blockUI({ message: '<img style="padding-top: 10px" src="css/ajax-loader.gif" /> <p>Loading result...</p>' }); 
    $("#loading-error-btn").button().click(function() {
	window.location.href = "/result.html?id=" + $("#loading-error input[type=text]").val();
    });
    $("#loading-error input[type=text]").keypress(function(event) {
	if ( event.which == 13 ) {
	    $("#loading-error-btn").click();
	}
    });
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
	    $("#build-information .build-information-bubble:first").fadeOut();
	}
    });

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
	    if(data.type == "error") {
		$.blockUI({message: $("#loading-error")});
	    } else {
		handleModel(data.data);
		handleResult(data.data);
		$.unblockUI();
	    }},
	error: function() {
	    $.blockUI({message: $("#loading-error")});
	}
    });

    function handleModel(data) {
	if(data.model.available) {
	    $("#model-tools").show();
	    $("#use-model").click(function() {
		$("#use-model-dialog").dialog({
		    modal: true,
		    autoResize:true,
		    width: 500,
		    position: {
			my: "center top",
			at: "center bottom",
			of: $("#top-panel")
		    },
		    open: function (event, ui) {
			loadFeatures(this, data);
		    }
		});		   
	    });
	    $("#download-model").attr("href", "/api/model/get/" + $.getUrl("id")).attr("download", data.file.name + "_model.rr");
	}
    }

    function loadFeatures(that, data) {
	var table = $("#use-model-dialog .attributes").html("");
	for(f in data.features) {
	    (function(feature) {
		table.append("<tr>" +
			     "<td class='attr-key'>" + feature.name + "</td>" + 
			     "<td class='attr-type'>" + feature.type + "</td>" + 
			     "<td class='attr-value'><input type='text' /></td>" +
			     "</tr>");
	    })(data.features[f])
	}
	$("#evaluate").click(function() {
	    rr.client("ws://" + window.location.host + "/api/evaluator", {
		message: function (data) {
		    console.log(data);
		},
		complete: function(data) {
		    $("#prediction-content").hide("blind").show("blind");
		    $("#prediction-result").html("");
		    var r = Raphael("prediction-result");
		    drawClassDistribution(r, {
			title: "Prediction probabilities",
			x: r.width/2,
			y: 150,
			legends: data.probabilities.map(function(e){ return e.class + " %%.%%" }),
			legendpos: "south",
			animate: animatePie,
			data: data.probabilities.map(function(e) { return e.prob })
		    });
		    $("#predicted")
			.html("")
			.append("<p>The most probable class is: <strong>" + data.prediction.class + "</strong></p>");
		},
		close_on_complete: true,
		payload: $.toJSON({
		    id: parseInt($.getUrl("id")),
		    example: collect_evaluation("#use-model-dialog")
		})
	    });
	});
    }

    function collect_evaluation(s) {
	var types = $.makeArray($(s + " .attr-type").map(function(){ console.log(this); return $(this).text(); }));
	types.push("Class");
	var names = $.makeArray($(s + " .attr-key").map(function(){ console.log(this); return $(this).text(); }));
	names.push("Class");
	var values = $.makeArray($(s + " .attr-value").map(function(){ 
	    var tmp = $(this).children().first().val();
	    if(tmp) {
		return tmp;
	    } else {
		alert("your are missing a value");
		throw "error!!";
	    }
	}));
	values.push("unkown");
	var combined = types.join() + "\r\n" + names.join() + "\r\n" + values.join() + "\r\n";
	console.log(combined);
	return combined;

/* "numeric, numeric, numeric, numeric, class\r\n"+
	    "Spetal Length, Sepal Widht, Petal Length, Petal Width, Class\r\n" +
	    "1.1,2.5,4.2,3.1,?\r\n";*/
	
    }

    function handleResult(data) {
	if(data.type == "cross-validation") {
	    handleFolds(data.folds);
	}
	handleInfo(data.file, {id: "#dataset"});
	handleInfo(data.learner, {id: "#learner"});
	handleInfo(data.evaluator, {id: "#evaluation"});		
	auc(data.predictions, data.folds[0].measures.auc);

	classStatistics(data.predictions, data.folds[0]);
    }

    function classStatistics(predictions, avg) {
	var r = Raphael("class-graphs");
	res = countPredictedClasses(predictions)
	drawClassDistribution(r, {
	    title: "Posterior Class Distribution",
	    x: r.width/2 + 300,
	    y: 150,
	    legends: res.labels.map(function(e) { return e + " (%%.%%)"; }),
	    animate: animatePie,
	    legendpos: "south",
	    data: res.data
	});

	drawClassDistribution(r, {
	    title: "Prior Class Distribution",
	    x: r.width/2 - 300,
	    y: 150,
	    legends: res.labels.map(function(e) { return e + " (%%.%%)"}),
	    legendpos: "south",
	    animate: animatePie,
	    data: predictions.classes.map(function(e) { return e.count; }) //res.slice(0)
	});

	drawAccuracy(r, avg.measures.accuracy, {
	    title: "Accuracy",
	    animate: animatePie,
	    x: r.width/2,
	    y: 150,
	    legends: ["Correctly classified (%%.%%)", 
		      "Incorrectly classified (%%.%%)"],
	    legendpos: "south"
	});

	r.text(r.width/2, 350, "Precision/Recall").attr({font: "20px Helvetica"});
	drawBar(r, avg.measures, {
	    x: r.width/2-150,
	    y: 360,
	    width: 300,
	    height: 200});
	    
    }

    function countPredictedClasses(predictions) {
	var tmp = {}, pp = predictions.predictions;
	for(c in predictions.classes) {
	    tmp[predictions.classes[c].class] = 0;
	}
	for(p in pp) {
	    var pre = pp[p].predictions[0]["class"];
	    tmp[pre] += 1;
	}
	res = [], labels = [];
	for(c in predictions.classes) {
	    var pre = tmp[predictions.classes[c].class];
//	    if(pre != undefined && pre > 1) {
		labels.push(predictions.classes[c].class);
		res.push(tmp[predictions.classes[c].class]);
//	    }	    
	}
	return { data: res, labels: labels };
    }

    function auc(predictions, avg) {
	var r = Raphael("auc-graph");
	var auc = r.auc(r.width / 2, 10, r.width*0.6, r.width*0.4, predictions, {
	    baseline: true,
	    "stroke-widht": 2,
	    legend_values: avg
	});
	auc.lines.hover(function() {
	    this.label[0].stop();
            this.label[0].attr({ r: 7.5 });
	}, function () {
            this.label[0].animate({ r: 5 }, 500, "bounce");
	});
    }

    function handleFolds(folds) {
	var avg = folds[0].measures;
	var r = Raphael("basic-graphs");

	drawAccuracy(r, avg.base_accuracy, {
	    title: "Base learner accuracy",
	    animate: animatePie,
	    x: r.width/2 + 150,
	    legends: ["Correctly classified by\n base learner (%%.%%)", 
		      "Incorrectly classified by\n base learner (%%.%%)"],
	    legendpos: "south"
	});
	drawAccuracy(r, avg.oob_base_accuracy, {
	    title: "OOB Base learner accuracy",
	    animate: animatePie,
	    x: r.width/2 - 150,
	    legends: ["Correctly classified OOB-example \n by base learner (%%.%%)", 
		      "Incorrectly classified OOB-example \n by base learner (%%.%%)"],
	    legendpos: "south"
	});


	var txt = r.text(r.width/2+150, 500 - 130, "Brier Score").attr({font: "20px Helvetica"});
	r.text(r.width/2+150, 500 - 115, "The smaller inner circle, the better").attr({font: "10px Helvetica"});
	var p = r.pointvalue(r.width/2+150, 500, 100, {
	    max: 2,
	    value: avg.brier,
	    color: "#df3e3e",
	    legend: "Brier score",
	});
	animatePointValue(p.center);

	var txt = r.text(r.width/2-150, 500 - 130, "Strength").attr({font: "20px Helvetica"});
	r.text(r.width/2-150, 500 - 115, "The larger inner circle, the better").attr({font: "10px Helvetica"});
	p = r.pointvalue(r.width/2-150, 500, 100, {
	    max: 1,
	    value: avg.strength,
	    color: "#5ca941",
	    legend: "Strenght"
	});
	animatePointValue(p.center);

	for(key in avg) {
	    $("#other .attributes").append(
		"<tr>" +
		"  <td class='attr-key'>" + key+ "</td>" +
		"  <td class='attr-value'>" + avg[key] + "</td>" + 		
		"</tr>");
	}
    }

    function drawClassDistribution(r, opts) {
	r.text(opts.x, opts.y-130, opts.title).attr({font: "20px Helvetica"});
	var pie = r.piechart(opts.x, opts.y, 100, opts.data, {
	    legend: opts.legends,
	    legendpos: opts.legendpos,
	    sort: false
	});
	opts.animate(pie);
			     
    }

    function drawAccuracy(r, a, opts) {
	opts.y = opts.y || 150;
	r.text(opts.x, opts.y-130, opts.title).attr({font: "20px Helvetica"});
	var pie = r.piechart(opts.x, opts.y, 100, [a, 1-a],
			     {
				 legend: opts.legends,
				 legendpos: opts.legendpos,
				 colors: ["#5ca941", "#df3e3e"]
			     });
	opts.animate(pie);
    }

    function animatePointValue(inner) {
	inner.hover(function() {
	    if(this.legend) {
		this.stop();
		this.scale(1.07, 1.07, this.cx, this.cy);
		this.legend[0].stop();
		this.legend[0].attr({r: 7.5});
	    }
	}, function() {
	    if(this.legend) {
		this.stop();
		this.legend[0].stop();
		this.legend[0].animate({ r: 5 }, 500, "bounce");
		this.animate({ transform: 's1 1 ' + this.cx + ' ' + this.cy }, 500, "bounce");
	    }
	});
    }

    function animatePie(pie) {
	pie.hover(function() {
	    this.sector.stop();
            this.sector.scale(1.07, 1.07, this.cx, this.cy);

            if (this.label) {
                this.label[0].stop();
                this.label[0].attr({ r: 7.5 });
            }
	}, function() {
	    this.sector.animate({ transform: 's1 1 ' + this.cx + ' ' + this.cy }, 500, "bounce");

            if (this.label) {
                this.label[0].animate({ r: 5 }, 500, "bounce");
            }
	});
	pie.each(function() {
	    if(this.label) {
		this.label.attr({font: "12px Helvetica"});
	    }
	});
    }

    function handleInfo(data, opts) {
	var attrs = $(opts.id + " .attributes");
	for(key in data) {
	    attrs.append(
		"<tr>" + 
		"  <td class='attr-key'>" + key + "</td>" +
		"  <td class='attr-value'>" + data[key] + "</td>" +
		"</tr>");
	}
    }

    function drawBar(r, data, opts) {
	var precision = [], recall = [], classes=[];
	for(key in data.precision) {
	    classes.push(key);
	    precision.push(data.precision[key]);
	}
	for(key in data.recall) {
	    recall.push(data.recall[key]);
	}
	fin = function () {
            this.flag = r.popup(this.bar.x, this.bar.y, this.bar.value.toFixed(2) || "0").insertBefore(this);
        },
        fout = function () {
            this.flag.animate({opacity: 0}, 300, function () {this.remove();});
        };
	var bar = r.barchart(opts.x, opts.y, opts.width, opts.height, [precision, recall], {
	    stacked: false,
	    max: 1
	});
	labels = opts.labels || classes;
        bar.labels = r.set();
        var i = 0;
        for (var j = 0; j < bar.bars[0].length; j++) {
            var totX = 0;
            for (i = 0; i < bar.bars.length; i++) {
                totX += bar.bars[i][j].x;
                y = bar.bars[0][j].y + bar.bars[0][j].h + 20;
            }
            x = totX / bar.bars.length;
            r.text(x, y, labels[j]).attr("font","12px Helvetica");
        }
	bar.hover(fin, fout);
	Raphael.g.axis(opts.x, opts.y+opts.height-21, opts.height-41, 0, 1, 10, 1, r);
	Raphael.g.axis(
	    opts.x, // 10 + gutter
	    opts.y+opts.height-21,
	    opts.width, null, null,
	    1, // number of steps 
	    0, [" "," "," "],
	    r);
	
	labels = ["Precision", "Recall"];
	bar.labels = r.set();
 	var nx = opts.x + opts.width/4; var h = opts.y + opts.height+25;
 	for( var i = 0; i < labels.length; ++i ) {
 	    var clr = bar.bars[i].items[0].attr("fill");
 	    bar.labels.push(r.set());
 	    bar.labels[i].push(r["circle"](nx + 5, h, 5).attr({fill: clr, stroke: "none"}));
 	    bar.labels[i].push(txt = r.text(nx + 20, h, labels[i]).attr({fill: "#000", "text-anchor": "start"}));
 	    nx += bar.labels[i].getBBox().width * 1.3;
 	};
    }
});
