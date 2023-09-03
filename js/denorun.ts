import { add } from "./denolib.ts";

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

adder();
env();
