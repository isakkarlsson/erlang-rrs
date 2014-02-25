(function() {
    function min_max(scores) {
        var min = 10000000,
            max = -10000000;
        for(key in scores) {
            var score = scores[key]
            if(score > max) {
                max = score;
            }
            if (score < min) {
                min = score
            } 
        }
        return {"min": min, "max": max}
    }

    function Ball(paper, start_x, start_y, width, height, old_scores, opts) {
        var MAX_WIDTH = width
        var RADIUS = opts.radius || 30
        var MIN_SIZE = opts.min_radius || 5
        var PADDING = opts.padding || 5
        var MAX = opts.max || 100
        m = min_max(old_scores)        
        var scores = []

        for(var key in old_scores) {
            score = old_scores[key]
            scores.push({
                "key": key,
                "score": (score - m.min)/(m.max - m.min)
            });
        }
        scores.sort(function (a, b) { return b.score - a.score })
        console.log(scores)
        var circles = []
        var row = 0

        var max_in_row = 0
        var x = PADDING
        var y = RADIUS*scores[max_in_row].score+40+PADDING
        for(var i = 0; i < scores.length && i < MAX; i++) {
            var size = RADIUS*scores[i].score+MIN_SIZE;
            var width = x + size + PADDING
            if(width+size > MAX_WIDTH-start_x) {
                x = RADIUS/2+RADIUS*scores[max_in_row].score*2
                y += RADIUS*scores[i].score + RADIUS*scores[max_in_row+1].score+PADDING
                max_in_row = i
            } else {     
                x = width
            }
            circles.push({
                "size": size,
                "text": scores[i].key,
                "score": scores[i].score,
                "x": start_x+x,
                "y": start_y+y
            });
            x += size
        }
        console.log(circles)
        var rainbow = new Rainbow(); 
        rainbow.setNumberRange(1, circles.length);
        rainbow.setSpectrum('#7fd738', '#f86f6f');
        for(var i = 0; i < circles.length; i++) {
            var c = circles[i]
            fin = function () {
                this.animate({"opacity": 1.0, "r": this.c.size+5}, 300)
                this.flag = paper.popup(this.c.x, 
                                        this.c.y-this.c.size-5, 
                                        this.c.text)
                    .insertBefore(this);
            },
            fout = function () {
                this.animate({"opacity": 0.8, "r": this.c.size}, 300)
                this.flag.animate({opacity: 0}, 300, function () {this.remove();});
            };
            p = paper.circle(c.x, c.y, c.size)
            p.c = c
            p.text = paper.text(c.x, c.y, c.text).attr({fill: "#000", "text-anchor": "center"})
            if(p.text.getBBox().width > c.size*2) {
                p.text.attr("text", "...")
            }
            p.hover(fin, fout)
            p.attr({
                "fill": '#'+rainbow.colourAt(i),
                "opacity": 0.8,
                "stroke-opacity": 0.5
            })
        }
    }

    var F = function(){};
    F.prototype = Raphael.g;
    Ball.prototype = new F;

    Raphael.fn.ball = function(x, y, width, height, scores, opts) {
        return new Ball(this, x, y, width, height, scores, opts);   
    };
}());
