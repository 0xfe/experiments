import Circle from './circle';
import Bouncer from './bouncer';

function main() {
    const container = document.getElementById( 'cont' );
    const width = container.getBoundingClientRect().width;
    const bouncer = new Bouncer(new Circle(300, 300, 30).createElement("cont"));
    bouncer.setBounds(100, width - 100);

    const startstop = document.getElementById("startstop");
    startstop.addEventListener("click", function(event) {
        bouncer.toggle();
    });
}

main();