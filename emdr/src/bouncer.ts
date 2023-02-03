import Circle from './circle';

export default class Bouncer {
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
        this.speed = 10;
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