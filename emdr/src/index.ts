function drawCircle() {
    const svgns = "http://www.w3.org/2000/svg";
    const container = document.getElementById( 'cont' );
    let circle = document.createElementNS(svgns, 'circle');
    circle.setAttribute("id", "circle1");
    circle.setAttributeNS(null, 'cx', "300");
    circle.setAttributeNS(null, 'cy', "300");
    circle.setAttributeNS(null, 'r', "50");
    circle.setAttributeNS(null, 'style', 'fill: none; stroke: blue; stroke-width: 1px;' );
    container.appendChild(circle);
}

function animate() {
    let cx = 10;
    let step = 2;

    const circle = document.getElementById('circle1');

    setInterval(function () {
        circle.setAttributeNS(null, 'cx', cx.toString())

        cx += step;
        if (cx > 800) {
            step = -2;
        } else if (cx < 20) {
            step = 2;
        }
    }, 20);
}

function main() {
  console.log("Hello world!")
  drawCircle();
  animate();
}

main();