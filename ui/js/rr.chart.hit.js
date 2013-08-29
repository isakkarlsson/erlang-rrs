(function() {
    function scatter(x) {
	var dir = Math.random() > 0.5 ? 1 : -1;
	return dir*Math.floor((Math.random()*x));
    }

    function Hit(paper, x, y, heigth, width, predictions, opts) {
	opts.scatter = opts.scatter || 20;
	opts.colors = opts.colors || this.colors;
	opts.decrease_scatter = opts.decrease_scatter || false;
	opts.labels_per_row = opts.labels_per_row || 7;
	opts.only_best = opts.only_best == undefined ? true : opts.only_best;

	var chart = paper.set();
	var classes = predictions.classes;
	var axis = paper.set();
	chart.push(axis);
	var classid = {};
	for(var i = 0; i < classes.length; i++) {
	    classid[classes[i].class] = i;
	}

	var colors = opts.colors;
	var transform = 90,
	    angle_step = (360*(1/classes.length));
	var gradeset = paper.set();
	var gradetext = paper.set();
	var gradeline = paper.set();

	chart.gradeset = gradeset;
	chart.gradetext = gradetext;

	if(angle_step - opts.scatter < 5 && opts.decrease_scatter) {
	    opts.scatter /= classes.length;
	}

	for(var g = 1; g <= 10; g++) {
	    var t = paper.set();
	    var grade = paper.circle(x, heigth/2, ((width/2)/10)*g-10);
	    var text = paper.text(x+width/2+20, ((heigth/2)/10)*(10-g)+10, g*10 + "%");
	    t.push(text);

	    text = paper.text(x+width/2+20, heigth-((heigth/2)/10)*(10-g)-10, g*10 + "%");
	    t.push(text);
	    grade.text = t;
	    gradetext.push(t);
	    gradeset.push(grade);
	}
	chart.push(gradeset);
	gradeset.attr({
	    "stroke-dasharray": "-",
	    "stroke-width": 1,
	    stroke: "#efefef",
	    fill: "#fff"
	}).toBack();
	gradetext.attr({
	    fill: "#333",
	    "font-size": 8
	})
	gradeline.attr({
	    "arrow-end": "block-wide-long",
	    "stroke-dasharray": "-",
	    "stroke-width": 1,
	    stroke: "#efefef"
	});
	
	var text_y = heigth+20,
	    text_x = x-width/2,
	    widest = 0;
	chart.axis = paper.set();
	for(var i=0; i < classes.length; i++){
	    var lineset = paper.set();
	    chart.axis.push(lineset);
	    var line = paper.path(["M", x, 1, "L", x, width/2]);
	    var box = paper.rect(x-opts.scatter, 10, opts.scatter*2, heigth/2-10).attr({
		fill: "#000",
		"fill-opacity": 0.02,
		stroke: "#000",
		"stroke-opacity": 0.04
	    });
	    lineset.line = line;
	    lineset.box = box;
	    lineset.push(box);
	    line.attr({"stroke-width": 2, "stroke": "#333", "stroke-opacity": 0.5, "stroke-dasharray": "-"});
	    lineset.push(line);
	    circle = paper.circle(x, 5, 4).attr({stroke: "black" , fill: colors[i] || "black"});
	    lineset.push(circle);
	    line.top = circle;
	    line.top.box = box;
	    box.points = paper.set();
	    lineset.push(box.points);

	    var label = paper.text(text_x, text_y, classes[i].class).attr({
		"text-anchor": "start"
	    });
	    var small = paper.circle(text_x-10, text_y, 6).attr({
		fill: colors[i],
		stroke: colors[i]
	    });

	    var text_w = label.getBBox().width + 30;
	    if(text_w > widest) {
		widest = text_w;
	    }
		
	    text_y += 17;
	    if(text_y >= heigth+18*opts.labels_per_row) {
		text_y = heigth+20;
		text_x += widest;
		widest = 0;
	    }

	    for(p in predictions.predictions) {
		(function(pred) {
		    if(classid[pred.real] == i) {
			for(var pi = 0; pi < pred.predictions.length; pi++) {
			    if(pi > 0 && opts.only_best) {
				break;
			    }
			    var pp = pred.predictions[pi]
			    if(pp.probability >= 0.14) {
				var cross = paper.circle(x+scatter(opts.scatter), 14+(heigth/2)*(1-pp.probability), 3)
				    .attr({
					fill: colors[classid[pp.class]],
					stroke: colors[classid[pp.class]],
					"fill-opacity": 0.0
				    });
				box.points.push(cross);
			    }
			}
		    }
		})(predictions.predictions[p]);
	    }	    

	    lineset.animate({transform:"r-" + transform  + "," + x + "," + (width/2)}, 0);
	    axis.push(line);

	    transform += angle_step;
	}

	return chart;	
	
    }
   
    var F = function() {};
    F.prototype = Raphael.g;
    Hit.prototype = new F;

    Raphael.fn.hitchart = function (x, y, width, height, predictions, opts) {
	return new Hit(this, x, y, width, height, predictions, opts);
    };

}());
