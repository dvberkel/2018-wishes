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

        const [story, links] = source.split('---');

        const content = template.story({
            story: md.toHTML(story)
        });
        const output_file = file.replace('story', 'html');
        const output_path = path.join(docs_path, output_file);
        fs.writeFileSync(output_path, content);
    });
});
