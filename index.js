const Handlebars = require('handlebars');
const fs = require('fs');

const template_content = fs.readFileSync('template/story.handlebars');
const template = Handlebars.compile(template_content.toString());

var result = template({
    story: "<p>You find yourself in a dark room.</p>"
});
console.log(result);
