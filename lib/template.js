const Handlebars = require('handlebars');
const fs = require('fs');

const sources = {
    'story': 'template/story.handlebars'
};

const template = {};
for (const key in sources) {
    const content = fs.readFileSync(sources[key]);
    template[key] = Handlebars.compile(content.toString());
}

module.exports = template;
