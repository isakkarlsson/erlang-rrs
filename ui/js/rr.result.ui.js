$(document).ready(function() {
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
		alert("could not find result");
	    } else {
		handleResult(data.data);
	    }
	}});

    function handleResult(data) {
	if(data.type == "cross-validation") {
	    handleFolds(data.folds);
	}
	$.ajax({
	    type: "get",
	    dataType: "json",
	    url: "api/machine-learning/get/" + data.learner.id,
	    success: function (l) {
		handleInfo(data.file, {id: "#dataset"});
		handleInfo(data.learner, {id: "#learner"});
		handleInfo(data.evaluator, {id: "#evaluation"});		
	    }
	});

	handlePredictions(data.predictions);
    }

    function handlePredictions(predictions) {
	var r = Raphael("class-graphs");

	var tmp = {}, pp = predictions.predictions;
	for(c in predictions.classes) {
	    tmp[predictions.classes[c]] = 0;
	}
	for(p in pp) {
	    var pre = pp[p].predictions[0]["class"];
	    tmp[pre] += 1;
	}
	res = [], legends = [];
	for(c in predictions.classes) {
	    var pre = tmp[predictions.classes[c]];
	    if(pre != undefined && pre > 1) {
		res.push(tmp[predictions.classes[c]]);
		legends.push(predictions.classes[c]);
	    }	    
	}
	drawClassDistribution(r, {
	    title: "Posterior Class Distribution",
	    x: r.width/2 + 150,
	    legends: $(legends).map(function() { return this + "(%%)"; }),
	    animate: animatePie,
	    legendpos: "east",
	    data: res
	});

	var tmp = {}, pp = predictions.predictions;
	for(c in predictions.classes) {
	    tmp[predictions.classes[c]] = 0;
	}
	for(p in pp) {
	    var pre = pp[p].real;
	    tmp[pre] += 1;
	}
	res = [], legends = [];
	for(c in predictions.classes) {
	    var pre = tmp[predictions.classes[c]];
	    if(pre > 1) {
		res.push(tmp[predictions.classes[c]]);
		legends.push(predictions.classes[c])
	    }	    
	}
	drawClassDistribution(r, {
	    title: "Prior Class Distribution",
	    x: r.width/2 - 150,
	    legends: $(legends).map(function() { return this + "(%%)"; }),
	    legendpos: "west",
	    animate: animatePie,
	    data: res.slice(0)
	});
	
	var r = Raphael("auc-graph");
	var auc = calculate_auc(predictions, res);
	drawAuc(r, {
	    x: r.width/2,
	    y: 10,
	    width: r.width * 0.6,
	    height: r.width * 0.4,
	    labels: ["Baseline"].concat(legends),
	    tp: [[0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]].concat(auc.tp),
	    fp: [[0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]].concat(auc.fp)
	});

    }

    function drawAuc(r, opts) {
	var line = r.linechart(opts.x-opts.width/2, opts.y, opts.width, opts.height, opts.fp, opts.tp, {
	    axis: "1 1 1 1",
	    axisxstep: 0,
	    axisystep: 0,
	    
	});
	line.lines.hover(function() {
	    this.label[0].stop();
            this.label[0].attr({ r: 7.5 });
	    this.label[1].attr({"font-weight": 800});
	}, function () {
            this.label[0].animate({ r: 5 }, 500, "bounce");
            this.label[1].attr({ "font-weight": 400 });
	});

	var labels = opts.labels;
	line.labels = r.set();
 	var x = opts.x-opts.width/2; var h = opts.y+opts.height+30;
 	for( var i = 0; i < labels.length; ++i ) {
 	    var clr = line.lines[i].attr("stroke");
 	    line.labels.push(r.set());
 	    line.labels[i].push(r["circle"](x + 5, h, 5)
 	                         .attr({fill: clr, stroke: "none"}));
 	    line.labels[i].push(txt = r.text(x + 20, h, labels[i])
 	                         .attr({fill: "#000", "text-anchor": "start"}));
 	    x += line.labels[i].getBBox().width * 1.3;
 	};
	for(var n=0; n < labels.length; n++) {
	    line.lines[n].label = line.labels[n]
	}
	
	line.labels.attr({font: "12px Helvetica"});
	line.axis.attr({font: "14px Helvetica"});
	line.lines.attr({"stroke-width": 3});
	line.lines[0].attr({'fill': 'none',  "stroke-dasharray": "-", "stroke-width": 2});
    }
    
    function findProb(p, c) {
	for(n in p) {
	    if(p[n]["class"] == c) {
		return p[n].probability;
	    }
	}
	return 0;
    }
    
    function calculate_auc(p, res) {
	var result = {
	    tp: [],
	    fp: []
	};
	var total = p.predictions.length;
	var sorted = p.predictions;
	console.log(p.classes);
	console.log(res);
	for(c in p.classes) {
	    var ctotal = res[c];
	    var x = sorted.sort(function(a, b) {
		var fa = findProb(a.predictions, p.classes[c]);
		var fb = findProb(b.predictions, p.classes[c]);
		return fb - fa;
	    });
	    console.log(sorted);
	    
	    var tp = [], fp = [];
	    var t = 0.0, f = 0.0;
	    for(n in sorted) {
		if(sorted[n].real == sorted[n].predictions[0]["class"] && sorted[n].real == p.classes[c]) {
		    t += 1;
		} else if (sorted[n].real != p.classes[c]){
		    f += 1;
		}
		tp.push(t/ctotal);
		var fpr = f/(total-ctotal);
		if(fpr > 1) {
		    console.log(f, (total-ctotal));
		}
		fp.push(fpr);
	    }
	    result.tp.push(tp);
	    result.fp.push(fp);
	}

	return result;	
    }

    function handleFolds(folds) {
	var avg = folds[0].measures;
	var r = Raphael("basic-graphs");
	drawAccuracy(r, avg.accuracy, {
	    title: "Accuracy",
	    animate: animatePie,
	    x: r.width/2,
	    legends: ["Correctly classified (%%.%%)", "Incorrectly classified (%%.%%)"],
	    legendpos: "south"
	});
	drawAccuracy(r, avg.base_accuracy, {
	    title: "Base learner accuracy",
	    animate: animatePie,
	    x: r.width/2 + 300,
	    legends: ["Correctly classified by\n base learner (%%.%%)", "Incorrectly classified by\n base learner (%%.%%)"],
	    legendpos: "south"
	});
	drawAccuracy(r, avg.oob_base_accuracy, {
	    title: "OOB Base learner accuracy",
	    animate: animatePie,
	    x: r.width/2 - 300,
	    legends: ["Correctly classified OOB-example \n by base learner (%%.%%)", "Incorrectly classified OOB-example \n by base learner (%%.%%)"],
	    legendpos: "south"
	});

	drawPointValueVisualizer(r, avg.brier, {
	    title: "Brier Score",
	    legend: "Brier score",
	    x: r.width/2+150,
	    y: 500,
	    multipler: 50,
	    max: 2,
	    type: "minimizer"
	});

	drawPointValueVisualizer(r, avg.strength, {
	    title: "Strength",
	    legend: "Strength",
	    x: r.width/2-150,
	    y: 500,
	    multipler: 100,
	    max: 1,
	    type: "maximizer"
	});

	for(key in avg) {
	    $("#other .attributes").append(
		"<tr>" +
		"  <td class='attr-key'>" + key+ "</td>" +
		"  <td class='attr-value'>" + avg[key] + "</td>" + 		
		"</tr>");
	}
    }

    function drawClassDistribution(r, opts) {
	r.text(opts.x, 20, opts.title).attr({font: "20px Helvetica"});
	var pie = r.piechart(opts.x, 150, 100, opts.data, {
	    legend: opts.legends,
	    legendpos: opts.legendpos
	});
	opts.animate(pie);
			     
    }

    function drawPointValueVisualizer(r, brier, opts) {
	var txt = r.text(opts.x, opts.y - 130, opts.title).attr({font: "20px Helvetica"});
	txt.node.setAttribute("class", "help");
	txt.node.setAttribute("help", "this is a help text");
	var outer = r.circle(opts.x, opts.y, opts.max*opts.multipler);
	outer.attr({fill: "#fff", stroke: "#000", "stroke-width": 3, "stroke-dasharray": "- ", opacity: .5});
	var centerX = outer.attr('cx') + outer.attr('width') / 2;
	var centerY = outer.attr('cy') + outer.attr('height') / 2;


	var color = undefined;
	if(opts.type == "maximizer") {
	    r.text(opts.x, opts.y - 115, "The larger inner circle, the better").attr({font: "10px Helvetica"});
	    color = "#5ca941";
	} else {
	    r.text(opts.x, opts.y - 115, "The smaller inner circle, the better").attr({font: "10px Helvetica"});
	    color = "#df3e3e";
	}

	var small = r.circle(centerX-50, centerY +  (opts.multipler*opts.max)+30, 6);
	small.attr({fill: color, "stroke": "none"});
	var text = r.text(centerX+15, centerY + (opts.multipler*opts.max)+30, opts.legend + " (" + brier.toFixed(3) + ")");
	text.attr({font: "12px Helvetica"});


	var inner = r.circle(centerX, centerY, opts.multipler*brier);
	inner.value=brier;
	inner.attr({fill: color, stroke: "#fff"});
	inner.hover(function() {
	    this.stop();
	    this.scale(1.07, 1.07, this.cx, this.cy);
	    small.stop();
	    small.attr({r: 7.5});
	    text.attr({"font-weight": 800});
	}, function() {
	    this.stop();
	    small.animate({ r: 5 }, 500, "bounce");
            text.attr({ "font-weight": 400 });
	    this.animate({ transform: 's1 1 ' + this.cx + ' ' + this.cy }, 500, "bounce");
	});
    }

    function drawAccuracy(r, a, opts) {
	r.text(opts.x, 20, opts.title).attr({font: "20px Helvetica"});
	var pie = r.piechart(opts.x, 150, 100, [a, 1-a],
			     {
				 legend: opts.legends,
				 legendpos: opts.legendpos,
				 colors: ["#5ca941", "#df3e3e"]
			     });
	opts.animate(pie);

    }

    function animatePie(pie) {
	pie.hover(function() {
	    this.sector.stop();
            this.sector.scale(1.07, 1.07, this.cx, this.cy);

            if (this.label) {
                this.label[0].stop();
                this.label[0].attr({ r: 7.5 });
                this.label[1].attr({ "font-weight": 800 });
            }
	}, function() {
	    this.sector.animate({ transform: 's1 1 ' + this.cx + ' ' + this.cy }, 500, "bounce");

            if (this.label) {
                this.label[0].animate({ r: 5 }, 500, "bounce");
                this.label[1].attr({ "font-weight": 400 });
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
});
