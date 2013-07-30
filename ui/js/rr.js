rr = (function () {
    var R = {};
    R.client = function (uri, opts) {
	var progress = opts.progress || function() {},
	complete = opts.complete || function() {},
	message = opts.message || function() {},
	error = opts.error || function() {},
	payload = opts.payload || undefined,
	close_on_complete = opts.close_on_complete || true;
	
	function Client(url) {
	    var that = this;
	    this.socket = new WebSocket(uri);
	    this.socket.onmessage = function (msg) {
		var data = $.parseJSON(msg.data);
		if(data.type == "progress") {
		    progress(data.data);
		} else if(data.type == "completed") {
		    complete(data.data);
		    if(close_on_complete)
			socket.close();
		} else if(data.type == "message") {
		    message(data.data);
		} else if(data.type == "error") {
		    error(data.data);
		} else {
		    error({reason: "unkown"})
		}
	    };
	    this.socket.onerror = function(evt) {
		error({reason: "internal"});
	    };
	    this.socket.onclose = function(evt) {
		that.online = false;
	    };
	    this.socket.onopen = function(evt) {
		if(payload) {
		    that.socket.send(payload);
		}
		that.online = true;
	    };

	    function send(msg) {
		if(this.online) {
		    this.socket.send(msg);
		}
		return this;
	    }

	    function close() {
		if(this.online) {
		    this.socket.close();
		}
		return this;
	    }
	}

	return new Client(uri);
    };



    return R;
}());
