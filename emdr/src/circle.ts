export default class Circle {
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
