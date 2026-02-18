# rat-redos

ReDoS analyzer for Node and browsers: check if a regex is vulnerable to **regular expression denial of service**.
`rat` is a *sound* tool, meaning that it is *mathematically proven* that it will not miss any ReDoS vulnerability.
Once `rat` determines that a regex is safe, it is *impossible* for an attacker to trigger an (exponential) ReDoS attack.
The paper is available [here](https://link.springer.com/chapter/10.1007/978-3-031-10363-6_6).

```js
const rat = require("rat-redos");

rat.hasRedos("(a+)+$");   // "Dangerous"
rat.hasRedos("abc");      // "Safe"
rat.hasRedos("[");        // "ParseError"
```

Returns one of `"Safe"`, `"Dangerous"`, or `"ParseError"`.
