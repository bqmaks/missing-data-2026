// Render equations at build time; the presentation needs no math CDN or runtime.
const fs = require('node:fs');
const katex = require('../assets/vendor/katex/katex.min.js');
const formulas = JSON.parse(fs.readFileSync(0, 'utf8'));
process.stdout.write(JSON.stringify(formulas.map(tex => katex.renderToString(tex, {
  throwOnError: true, displayMode: true, output: 'htmlAndMathml', strict: 'error'
}))));
