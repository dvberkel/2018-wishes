var template = require('./lib/template');

var result = template.story({
    story: "<p>You find yourself in a dark room.</p>"
});
console.log(result);
