import Circle from './circle';
import Bouncer from './bouncer';

import './static/index.css'

function main() {
    const circlebox = document.getElementById( 'circlebox' );
    const width = circlebox.getBoundingClientRect().width;
    const bouncer = new Bouncer(new Circle(300, 300, 30).createElement("circlebox"));
    bouncer.setBounds(100, width - 100);

    const startstopEl = document.getElementById("startstop") as HTMLButtonElement;
    startstopEl.addEventListener("click", function() {
        bouncer.toggle();
        startstopEl.innerHTML = bouncer.running ? "Stop" : "Start";
    });

    const speedEl = document.getElementById("speed") as HTMLInputElement;
    speedEl.addEventListener('change', function() {
        bouncer.setSpeed(Number(speedEl.value));
    });
}

main();