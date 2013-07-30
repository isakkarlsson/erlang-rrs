$(document).ready(function() {
    var menu = [
	{
	    id: "build",
	    icon: "M16,1.466C7.973,1.466,1.466,7.973,1.466,16c0,8.027,6.507,14.534,14.534,14.534c8.027,0,14.534-6.507,14.534-14.534C30.534,7.973,24.027,1.466,16,1.466zM24.386,14.968c-1.451,1.669-3.706,2.221-5.685,1.586l-7.188,8.266c-0.766,0.88-2.099,0.97-2.979,0.205s-0.973-2.099-0.208-2.979l7.198-8.275c-0.893-1.865-0.657-4.164,0.787-5.824c1.367-1.575,3.453-2.151,5.348-1.674l-2.754,3.212l0.901,2.621l2.722,0.529l2.761-3.22C26.037,11.229,25.762,13.387,24.386,14.968z",
	    href: "index.html",
	    title: "Build Model"
	},
	{
	    id: "inspect",
	    icon: "M15.583,15.917l1.648-10.779C16.692,5.056,16.145,5,15.583,5C9.554,5,4.666,9.888,4.666,15.917c0,6.029,4.888,10.917,10.917,10.917S26.5,21.946,26.5,15.917c0-0.256-0.021-0.507-0.038-0.759L15.583,15.917zM19.437,3.127l-1.648,10.779l10.879-0.759C28.313,8.026,24.436,3.886,19.437,3.127z",
	    href: "result.html",
	    title: "Inspect model"
	}
    ];
    menu.reverse();
    for(key in menu) {
	var item = menu[key];
	var id = item.id + "-rr-menu";
	$("#top-panel").prepend(
	    '<a href="' + item.href +'" style="float:left; margin-left: 10px;' +
		' margin-top:2px; width: 33px; height: 33px;" id="' + id + '"' +
		'title="'+item.title+'"></a>');

	var r = Raphael(id);
	var icon = r.path(item.icon).attr({fill: "#fff", stroke: "none"});
	var fin = function() {
	    this.animate({fill: "#f59e3b"}, 500);
	}
	var fout = function() {
	    this.stop();
	    this.animate({fill: "#fff"}, 500);
	}
	icon.hover(fin, fout);
    }
});
