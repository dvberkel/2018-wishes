const fs = require('fs');
const path = require('path');
const md = require('markdown').markdown;

const template = require('./lib/template');

const content_path = 'content';
const docs_path = 'docs';
fs.readdir(content_path, function(error, files){
    if (error) { throw error; }
    files.forEach(function(file){
        const source_path = path.join(content_path, file);
        const source = fs.readFileSync(source_path, { encoding: 'utf-8' });

        const [story, links, scripts] = source.split('---');

        var directions = undefined;
        if (links) {
            const tree = md.parse(links);
            transform_links(tree);
            directions = md.renderJsonML(md.toHTMLTree(tree));
        }

        const content = template.story({
            story: md.toHTML(story),
            directions: directions,
            scripts: scripts
        });
        const output_file = file.replace('story', 'html');
        const output_path = path.join(docs_path, output_file);
        fs.writeFileSync(output_path, content);
    });
});

function transform_links(jsonml) {
    if (jsonml[0] === 'bulletlist') {
        const items = jsonml.slice(1);
        items.forEach(transform_links);
    }
    else if (jsonml[0] === 'link') {
        jsonml[1].href = jsonml[1].href + '.html';
    }
    else if (Array.isArray(jsonml[1])) {
        transform_links(jsonml[1]);
    }
    else {
        /* do nothing */
    }
}
