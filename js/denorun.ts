import { add } from "./denolib.ts";
import chalk from "npm:chalk@5.3";

// deno run --allow-all denorun.ts

// deno run --allow-net="deno.com" ...
async function _dumpSite(site: string) {
  const res = await fetch(site); // https://deno.com
  const body = await res.text();

  console.log(body);
}

function adder() {
  console.log(add(1, 2));
}

// deno run --allow-env ...
function env() {
  console.log(Deno.env.get("HOME"));
}

// deno run --allow-all ...
function hello_color() {
  // Seems like you need --allow-all for terminal colors to work
  console.log(chalk.green("Hello"), chalk.blue("World!"));
}

adder();
env();
hello_color();
