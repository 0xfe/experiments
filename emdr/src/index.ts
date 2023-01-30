function drawCircle() {
    const svgns = "http://www.w3.org/2000/svg";
    const container = document.getElementById( 'cont' );
    console.log("cont:", container.getBoundingClientRect());
    let circle = document.createElementNS(svgns, 'circle');
    circle.setAttribute("id", "circle1");
    circle.setAttributeNS(null, 'cx', "300");
    circle.setAttributeNS(null, 'cy', "300");
    circle.setAttributeNS(null, 'r', "30");
    circle.setAttributeNS(null, 'style', 'fill: red; stroke: gray; stroke-width: 2px;' );
    container.appendChild(circle);
}

function animate(speed: number): NodeJS.Timer  {
    let cx = 300;
    let step = speed;
    const container = document.getElementById( 'cont' );
    const width = container.getBoundingClientRect().width;

    const circle = document.getElementById('circle1');

    const timer = setInterval(function () {
        circle.setAttributeNS(null, 'cx', cx.toString())

        cx += step;
        if (cx > width - 100) {
            step = -speed;
        } else if (cx < 100) {
            step = speed;
        }
    }, 5);

    return timer;
}

function main() {
  const step = 10;
  drawCircle();
  let timer = animate(step);

  const startstop = document.getElementById("startstop");
  startstop.addEventListener("click", function(event) {
    if (timer) {
        clearInterval(timer);
        timer = null;
    } else {
        timer = animate(step);
    }
  });
}

main();