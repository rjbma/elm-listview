import './main.css';
import './gridHtmlTable.css';
import { Elm } from './MainDemo.elm';
import * as serviceWorker from './serviceWorker';

Elm.MainDemo.init({
  node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
