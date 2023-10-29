// Lifted from https://brandur.org/live-reload
function connect() {
    var socket = new WebSocket("ws://localhost:5001/");

    socket.onclose = function(event) {
        console.log("Websocket connection closed or unable to connect; " +
                    "starting reconnect timeout");

        // Allow the last socket to be cleaned up.
        socket = null;

        // Set an interval to continue trying to reconnect periodically until we succeed.
        setTimeout(function() {
            connect();
        }, 5000)
    }

    socket.onmessage = function(event) {
        var data = JSON.parse(event.data);
        switch(data.type) {
        case "build_complete":
            // 1000 = "Normal closure" and the second parameter is a human-readable reason.
            socket.close(1000, "Reloading page after receiving build_complete");

            console.log("Reloading page after receiving build_complete");
            location.reload(true);

            break;

        default:
            console.log(`Don't know how to handle type '${data.type}'`);
        }
    }
}

connect();
