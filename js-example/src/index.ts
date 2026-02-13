const rat = require("../../_build/default/src/web/main.bc.js") as typeof import("./rat");

const regexes = [
  { source: "(a+)+$", label: "classic ReDoS" },
  { source: "abc", label: "safe literal" },
  { source: "(a|a)*$", label: "ambiguous alternation" },
];

for (const { source, label } of regexes) {
  console.log(`--- ${label}: /${source}/ ---`);
  const result = rat.analyze(source, "match");
  if (result.error) {
    console.log(`  Error: ${result.error}`);
  } else if (result.vulnerable) {
    console.log(`  Vulnerable!`);
    console.log(`  Attack language: ${result.attackLanguage}`);
    if (result.exploit) {
      console.log(`  Exploit: ${result.exploit}`);
      console.log(`  Example: ${result.example}`);
    }
  } else {
    console.log(`  Safe`);
  }
  console.log();
}
