import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Main.embed(document.getElementById('root'));

let canvas

document.addEventListener('mousemove', function(evt) {
  if (!canvas) {
    canvas = document.querySelector('canvas')
  }

  if (!canvas) {
    return
  }

  const { left, top } = canvas.getBoundingClientRect()

  console.log()

  app.ports.moveBar.send([
    event.clientX - left,
    - event.clientY
  ])
}, false);

registerServiceWorker();
