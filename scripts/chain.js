var esprima = require('esprima');
var escodegen = require('escodegen');

var fs = require('fs');

var filepath = process.argv[2];
var filecontent = fs.readFileSync(filepath, "utf8", function (content) {
	console.log(content);
});

var ast = esprima.parse('var answer = 42');
var json = JSON.stringify(ast, null, 4);


console.log(ast);
console.log("\n");

console.log(process.argv);

var js = escodegen.generate(ast);

console.log(js);
