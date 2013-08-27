(function() {
    function scatter(x) {
	var dir = Math.random() > 0.5 ? 1 : -1;
	return dir*Math.floor((Math.random()*x)+1);
    }

    function Hit(paper, x, y, heigth, width, predictions, opts) {
	opts.scatter = opts.scatter || 20;
	var chart = paper.set();
	var classes = predictions.classes;
	var axis = paper.set();
	chart.push(axis);
	var classid = {};
	for(var i = 0; i < classes.length; i++) {
	    classid[classes[i].class] = i;
	}
	var colors = this.colors;

	var transform = 90;
	var gradeset = paper.set();
	var gradetext = paper.set();
	var gradeline = paper.set();
	for(var g = 1; g <= 10; g++) {
	    var grade = paper.circle(width/2, heigth/2, ((width/2)/10)*g-10);
	    var text = paper.text(width+20, ((heigth/2)/10)*g-10, (10-g)*10+10 + "%");
	    var l = paper.path(["M", width/2, ((heigth/2)/10)*g-10, "L", width+10, ((heigth/2)/10)*g-10]);
	    gradeline.push(l);
	    gradetext.push(text);

	    text = paper.text(width+20, heigth-((heigth/2)/10)*g+10, (10-g)*10+10 + "%");
	    l = paper.path(["M", width/2, heigth-((heigth/2)/10)*g+10, "L", width+10, heigth-((heigth/2)/10)*g+10]);
	    gradeline.push(l);
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
	
	for(var i=0; i < classes.length; i++){
	    var lineset = paper.set();
	    var line = paper.path(["M", width/2, 1, "L", width/2, width/2]);

	    line.attr({"stroke-width": 2});
	    lineset.push(line);
	    circle = paper.circle(width/2, 5, 4).attr({stroke: "black" , fill: this.colors[i] || "black"});
	    lineset.push(circle);
	    var points = paper.set();
	    lineset.push(points);

	    var label = paper.text(width + 80, 40+17*i, classes[i].class);
	    var small = paper.circle(width + 55, 40+17*i, 6).attr({
		fill: colors[i],
		stroke: colors[i]
	    });
		

	    for(p in predictions.predictions) {
		(function(pred) {
		    if(classid[pred.real] == i) {
			var cross = paper.circle(width/2+scatter(opts.scatter), 13+(heigth/2)*(1-pred.predictions[0].probability), 3)
			    .attr({
				stroke: colors[classid[pred.predictions[0].class]]
			    });
			points.push(cross);
		    }
		})(predictions.predictions[p]);
	    }	    

	    lineset.animate({transform:"r-" + transform  + "," + (heigth/2) + "," + (width/2)}, 200);
	    axis.push(line);

	    transform += (360*(1/classes.length));
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
