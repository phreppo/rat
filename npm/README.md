# rat-redos

ReDoS analyzer for Node and browsers: check if a regex is vulnerable to **regular expression denial of service**.

```js
const rat = require("rat-redos");

rat.hasRedos("(a+)+$");   // "Dangerous"
rat.hasRedos("abc");      // "Safe"
rat.hasRedos("[");        // "ParseError"
```

Returns one of `"Safe"`, `"Dangerous"`, or `"ParseError"`.

## Install

```bash
npm install rat-redos
```
