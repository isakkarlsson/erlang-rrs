$(document).ready(function() {
    $.blockUI({ message: '<div id="page-progress" style="margin: 10px"></div><p>Loading result...</p>' });
    $("#page-progress").progressbar({
	value: 0
    });
    $("#loading-error-btn").button().click(function() {
	window.location.href = "/result.html?id=" + $("#loading-error input[type=text]").val();
    });
    $("#loading-error input[type=text]").keypress(function(event) {
	if ( event.which == 13 ) {
	    $("#loading-error-btn").click();
	}
    });
    $("#machine-learner-message").hide();
    $("#hide-show-left").click(function() {
	$("#machine-learner-message").show("slide");	
	$(this).hide();
    });
    $("#boxclose").click(function() {
	$("#hide-show-left").delay(600).show("fade");
	$("#machine-learner-message").hide("slide");
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
	xhr: function () {
	    var xhr = new window.XMLHttpRequest();
	    xhr.addEventListener("progress", function(evt){
		if (evt.lengthComputable) {
		    var pc = evt.loaded / evt.total;
		    $("#page-progress").progressbar("value", pc*100);
		    if(pc == 1) {
			$("#page-progress").progressbar("option", "value", false);
		    }
		}
	    }, false);
	    return xhr;
	},
	type: "get",
	dataType: "json",
	url: "api/result/get/" + $.getUrl("id"),
	success: function (data) {
	    if(data.type == "error") {
		$.blockUI({message: $("#loading-error")});
	    } else {
		handleModel(data.data);
		handleResult(data.data);
		console.log(data);
		$("title").text("rrs - result for '" + data.data.file.name + "'");
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
	$("#scatter-slide").slider({
	    range: "min",
	    value: 20,
	    max: Math.round(360*(1/data.predictions.classes.length))-6,
	    min: 1,
	    step: 1,
	    slide: function(event, ui) {
		hitchart(data.predictions, ui.value, $("#only-predictions").prop("checked"));
	    }	
	});
	$("#only-predictions").click(function (e) {
	    hitchart(data.predictions, $("#scatter-slide").slider("value"), $(this).prop("checked"));
	});
	hitchart(data.predictions, $("#scatter-slide").slider("value"), $("#only-predictions").prop("checked"));
	var v = data.predictions.predictions.length > 100 ? 100 : data.predictions.predictions.length;
	$("#histogram-slide-count").text(v + "/" + data.predictions.predictions.length);
	console.log(v);
	$("#histogram-slide").slider({
	    range: "min",
	    value: v,
	    max: data.predictions.predictions.length,
	    min: 10,
	    step: 10,
	    slide: function(event, ui) {
		$("#histogram-slide-count").text(ui.value + "/" + data.predictions.predictions.length);
		predictionHistogram(data.predictions, ui.value);
	    }
	});
	predictionHistogram(data.predictions, v);
    }

    function predictionHistogram(pred, max) {
	$("#histogram").html("");
	var p = getRandomSubarray(pred.predictions, max);
	var hist = [];
	for(var i = 0; i < p.length; i++) {
	    var ex = p[i], h_ex = [0];
	    var votes = ex.predictions[0].votes, pos = 0, neg = 0;
	    for(var v = 0; v < votes.length; v++) {
		if(votes[v] == 1) {
		    pos += 1;
		} else {
		    neg += 1;
		}
		if(v % 10 == 0) {
		    h_ex.push(pos/(neg+pos));
		}
	    }
	    hist.push(h_ex);
	}
	console.log(seq(0, hist[0].length*10-10, 10));
	console.log(hist);
	var r = Raphael("histogram");
	var c = r.linechart(40, 10, 620, 500, seq(0, hist[0].length*10-10, 10), hist, {
	    axis: "0 0 1 1",
	    colors: rainbow_seq(max),
	    axisxstep: 10,
	    smooth: true
	});
	c.lines.attr({"stroke-width": 1});
    }

    function seq(start, end, step) {
	var r = [];
	for(var i = start; i <= end; i+=step) {
	    r.push(i);
	}
	return r;
    }
    
    function rainbow_seq(total) {
	var l =[];
	for(i = 1; i <= total; i++) {
	    l.push(rainbow(total, i));
	}
	return l;
    }

    // http://stackoverflow.com/questions/11935175/sampling-a-random-subset-from-an-array
    function getRandomSubarray(arr, size) {
	if(size >= arr.length) {
	    return arr;
	}
	var shuffled = arr.slice(0), i = arr.length, min = i - size, temp, index;
	while (i-- > min) {
            index = Math.floor(i * Math.random());
            temp = shuffled[index];
            shuffled[index] = shuffled[i];
            shuffled[i] = temp;
	}
	return shuffled.slice(min);
    }
    
    function rainbow(numOfSteps, step) {
	// This function generates vibrant, "evenly spaced" colours
	// (i.e. no clustering). This is ideal for creating easily
	// distinguishable vibrant markers in Google Maps and other
	// apps.  Adam Cole, 2011-Sept-14 HSV to RBG adapted from:
	// http://mjijackson.com/2008/02/rgb-to-hsl-and-rgb-to-hsv-color-model-conversion-algorithms-in-javascript
	var r, g, b;
	var h = step / numOfSteps;
	var i = ~~(h * 6);
	var f = h * 6 - i;
	var q = 1 - f;
	switch(i % 6){
        case 0: r = 1, g = f, b = 0; break;
        case 1: r = q, g = 1, b = 0; break;
        case 2: r = 0, g = 1, b = f; break;
        case 3: r = 0, g = q, b = 1; break;
        case 4: r = f, g = 0, b = 1; break;
        case 5: r = 1, g = 0, b = q; break;
	}
	var c = "#" + ("00" + (~ ~(r * 255)).toString(16)).slice(-2) + 
	    ("00" + (~ ~(g * 255)).toString(16)).slice(-2) + 
	    ("00" + (~ ~(b * 255)).toString(16)).slice(-2);
	return (c);
    }
    function hitchart(pred, scatter, only_best) {
	console.log("only best", only_best);
	$("#hit-graph").html("");
	var r = Raphael("hit-graph");
	var hit = r.hitchart(r.width/2, r.width/2, 400, 400, pred, {
	    scatter: scatter,
	    decrease_scatter: true,
	    labels_per_row: 5,
	    only_best: only_best
	});
	for(var i = 0; i < hit.gradeset.length; i++) {
	    hit.gradeset[i].hover(function() {
		this.text.attr({"font-size": "14px"});
	    }, function() {
		this.text.attr({"font-size": "8px"});
	    });
	}
	for(var i = 0; i < hit.axis.length; i++) {
	    hit.axis[i].box.hover(function() {
		this.points.animate({r: 4, "fill-opacity": 0.5}, 100);
	    }, function() {
		this.points.animate({r: 3, "fill-opacity": 0.0}, 100);
	    });
	}
	for(var i = 0; i < hit.axis.length; i++) {
	    hit.axis[i].line.top.hover(function() {
		console.log(this);
		this.box.points.animate({r: 4, "fill-opacity": 0.5}, 100);
	    }, function() {
		this.box.points.animate({r: 3, "fill-opacity": 0.0}, 100);
	    });
	}
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
	var out = function(avg) {
	    if(typeof avg == "number") {
		return avg.toFixed(5);
	    }

	    var str = "";
	    for(key in avg) {
		str += "<strong>" + key + "</strong>: " + avg[key].toFixed(5) + "<br />";
	    }
	    return str;
	    
	}
	
	for(key in avg) {
	    $("#other .attributes").append(
		"<tr>" +
		"  <td class='attr-key'>" + key+ "</td>" +
		"  <td class='attr-value'>" + out(avg[key]) + "</td>" + 		
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
				 sort: false,
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
