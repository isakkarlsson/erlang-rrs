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

	    handleInfo(data.file, {id: "#dataset"});
	    handleInfo(data.learner, {id: "#learner"});
	    handleInfo(data.evaluator, {id: "#evaluation"});
	}});

    function handleFolds(folds) {
	var avg = folds[0].measures;
	var r = Raphael("basic-graphs");
	drawAccuracy(r, avg.accuracy, {
	    title: "Accuracy",
	    x: r.width/2,
	    legends: ["Correctly classified (%%.%%)", "Incorrectly classified (%%.%%)"],
	    legendpos: "south"
	});
	drawAccuracy(r, avg.base_accuracy, {
	    title: "Base learner accuracy",
	    x: r.width/2 + 300,
	    legends: ["Correctly classified by\n base learner (%%.%%)", "Incorrectly classified by\n base learner (%%.%%)"],
	    legendpos: "south"
	});
	drawAccuracy(r, avg.oob_base_accuracy, {
	    title: "OOB Base learner accuracy",
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
	    $("#auc .attributes").append(
		"<tr>" +
		"  <td class='attr-key'>" + key+ "</td>" +
		"  <td class='attr-value'>" + avg[key] + "</td>" + 		
		"</tr>");
	}
    }

    function drawPointValueVisualizer(r, brier, opts) {
	r.text(opts.x, opts.y - 130, opts.title).attr({font: "20px Helvetica"});
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
