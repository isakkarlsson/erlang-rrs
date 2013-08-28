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

	console.log("hej", Math.abs(angle_step - opts.scatter), angle_step, opts.scatter);
	if(angle_step - opts.scatter < 5 && opts.decrease_scatter) {
	    opts.scatter /= classes.length;
	}

	for(var g = 1; g <= 10; g++) {
	    var grade = paper.circle(x, heigth/2, ((width/2)/10)*g-10);
	    var text = paper.text(x+width/2+20, ((heigth/2)/10)*g-10, (10-g)*10+10 + "%");
	    gradetext.push(text);

	    text = paper.text(x+width/2+20, heigth-((heigth/2)/10)*g+10, (10-g)*10+10 + "%");
	    gradetext.push(text);

	    gradeset.push(grade);
	}
	chart.push(gradeset);
	gradeset.attr({
	    "stroke-dasharray": "-",
	    "stroke-width": 1,
	    stroke: "#efefef",
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
	for(var i=0; i < classes.length; i++){
	    var lineset = paper.set();
	    var line = paper.path(["M", x, 1, "L", x, width/2]);

	    line.attr({"stroke-width": 2, "stroke": "#333"});
	    lineset.push(line);
	    circle = paper.circle(x, 5, 4).attr({stroke: "black" , fill: colors[i] || "black"});
	    lineset.push(circle);
	    var points = paper.set();
	    lineset.push(points);

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
			var cross = paper.circle(x+scatter(opts.scatter), 14+(heigth/2)*(1-pred.predictions[0].probability), 3)
			    .attr({
				stroke: colors[classid[pred.predictions[0].class]]
			    });
			points.push(cross);
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
