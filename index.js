var template = require('./lib/template');
var md = require('markdown').markdown;

var result = template.story({
    story: md.toHTML('you find yourself in a *dark* room.')
});
console.log(result);
