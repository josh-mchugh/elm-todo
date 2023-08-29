import './style.css';
import { Elm } from './src/Main.elm';

const STATE_STORE_KEY = 'elm-todo-save';
const storedState = localStorage.getItem(STATE_STORE_KEY);
const startingState = storedState ? JSON.parse(storedState) : null;

const app = Elm.Main.init({
    flags: startingState,
    node: document.getElementById("app")
});

app.ports.setStorage.subscribe(state => {
    localStorage.setItem(STATE_STORE_KEY, JSON.stringify(state));
});
