import {Elm} from './Main.elm';

const apiUrl = process.env.CONNECT_FOUR_WEB_URL;
const socketUrl = process.env.CONNECT_FOUR_SOCKET_URL;

const app = Elm.Main.init({flags: apiUrl});

let connection = null;

function close() {
  if (connection && connection.close) {
    connection.close();
    connection = null;
  }
}

console.log(app.ports);

//app.ports?.close.subscribe(close);

app.ports?.connect.subscribe((message) => {
  close();

  connection = new WebSocket(socketUrl);

  connection.onmessage = (event) => {
    app.ports?.receive.send(JSON.parse(event?.data));
  }

  connection.onopen = () => {
    connection.send(JSON.stringify(message));
  };
});

//app.ports?.send.subscribe((message) => {
  //if (connection && connection.send) {
    //connection.send(message);
  //}
//});
