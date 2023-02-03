class Circle {
    x: number;
    y: number;
    radius: number;
    style: string;
    el: SVGElement;

    constructor(x:number, y:number, radius: number) {
        this.x = x;
        this.y = y;
        this.radius = radius;
        this.style = 'fill: red; stroke: gray; stroke-width: 2px;';
    }

    setStyle(style: string) {
        this.style = style;
    }

    createElement(containerEl: string) {
        const svgns = "http://www.w3.org/2000/svg";
        this.el = document.createElementNS(svgns, 'circle');
        let container = document.getElementById(containerEl);
        this.el.setAttribute("id", "circle1");
        this.el.setAttributeNS(null, 'cx', this.x.toString());
        this.el.setAttributeNS(null, 'cy', this.y.toString());
        this.el.setAttributeNS(null, 'r', this.radius.toString());
        this.el.setAttributeNS(null, 'style', this.style);
        container.appendChild(this.el);
        return this;
    }

    moveTo(x: number, y: number) {
        this.x = x;
        this.y = y;
        this.el.setAttributeNS(null, 'cx', x.toString());
        this.el.setAttributeNS(null, 'cy', y.toString());
    }
}

class Bouncer {
    circle: Circle;
    x: number;
    minX: number;
    maxX: number;
    speed: number;
    timer: NodeJS.Timer;
    running: boolean;

    constructor(circle: Circle) {
        this.circle = circle;
        this.x = circle.x;
        this.minX = circle.x;
        this.maxX = circle.x + 20;
        this.speed = 20;
        this.running = false;
    }

    setBounds(minX: number, maxX: number) {
        this.minX = minX;
        this.maxX = maxX;
    }

    start() {
        let step = this.speed;
        const that = this;
        const interval_ms = 5;
        this.timer = setInterval(function() {
            that.circle.moveTo(that.x, that.circle.y);
            that.x += step;
            if (that.x > that.maxX) {
                step = -that.speed;
            } else if (that.x < that.minX) {
                step = that.speed;
            }
        }, interval_ms);
        this.running = true;
    }

    stop() {
        clearInterval(this.timer);
        this.running = false;
    }

    toggle() {
        this.running ? this.stop() : this.start();
    }
}

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