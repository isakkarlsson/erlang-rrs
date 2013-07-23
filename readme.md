Protocol:

JSON (websocket) api/rf:
     Send: {"method": "init"} 
     Receive: {"method": "init",
               "options": [{"key": options()}] where options() is either "numeric" or a list of values}

     Send: {"method": "build",
            "options": [{"key": value()}] where value() correspond to the options given}
     Receive: {"method", "started", "id": number()}
     Receive: {"method": "progress", "value": 0, "id": number()} where number is increasing

     Continues until:
     {"method": "done", "id": number()}
	       

	