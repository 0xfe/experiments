import { add } from "./denolib.ts";
import { assertEquals } from "https://deno.land/std@0.201.0/assert/mod.ts";

Deno.test("adder test", () => {
  assertEquals(add(1, 2), 3);
})