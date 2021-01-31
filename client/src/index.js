import {Elm} from './Main.elm';

const apiUrl = process.env.CONNECT_FOUR_WEB_URL;

Elm.Main.init({
  flags: apiUrl,
});
