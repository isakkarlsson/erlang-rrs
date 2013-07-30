(function () {
    function Point(paper, x, y, radius, opts) {
	var that = this;

	point = paper.set();
	var outer = paper.circle(x, y, radius);
	outer.attr({fill: "#fff", stroke: "#000", "stroke-width": 3, "stroke-dasharray": "- ", opacity: .5});
	var centerX = outer.attr('cx') + outer.attr('width') / 2;
	var centerY = outer.attr('cy') + outer.attr('height') / 2;

	point.outer = outer;
	point.push(outer);
	if(opts.legend) {
	    var set = paper.set();
	    var small = paper.circle(centerX-radius/2, centerY + radius+30, 6);
	    small.attr({fill: opts.color, "stroke": "none"});
	    var text = paper.text(centerX+15, centerY + radius+30, opts.legend + " (" + opts.value.toFixed(3) + ")");
	    text.attr({font: "12px Helvetica"});	    
	    set.push(small);
	    set.push(text);
	    point.push(set);
	    point.legend = set;
	}
	
	var inner = paper.circle(centerX, centerY, radius*(opts.value/opts.max));
	inner.value = opts.value;
	inner.attr({fill: opts.color, stroke: "#fff"});
	point.center = inner;
	point.center.legend = point.legend;
	point.push(inner);

	return point;
    }

    var F = function(){};
    F.prototype = Raphael.g;
    Point.prototype = new F;
    
    Raphael.fn.pointvalue = function(x, y, radius, opts) {
	return new Point(this, x, y, radius, opts);	
    };

}());
