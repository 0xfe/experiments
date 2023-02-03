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

    setSpeed(speed: number) {
        this.speed = speed;
    }

    start() {
        if (this.running) return;

        let direction = 1;
        const interval_ms = 5;

        this.timer = setInterval(() => {
            this.circle.moveTo(this.x, this.circle.y);
            this.x += this.speed * direction;
            if (this.x > this.maxX) {
                direction = -1;
            } else if (this.x < this.minX) {
                direction = 1;
            }
        }, interval_ms);
        this.running = true;
    }

    stop() {
        if (!this.running) return;

        clearInterval(this.timer);
        this.running = false;
    }

    toggle() {
        this.running ? this.stop() : this.start();
    }
}