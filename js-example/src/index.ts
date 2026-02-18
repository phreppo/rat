import type { Rat, RedosResult } from "./rat";
const rat: Rat = require("../../_build/default/src/web/main.bc.js");

const regexes = [
  { source: "(a+)+$", label: "classic ReDoS" },
  { source: "abc", label: "safe literal" },
  { source: "(a|a)*$", label: "ambiguous alternation" },
];

for (const { source, label } of regexes) {
  console.log(`--- ${label}: /${source}/ ---`);
  const result: RedosResult = rat.hasRedos(source);
  console.log(`  ${result}`);
  console.log();
}
