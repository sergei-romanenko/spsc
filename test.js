var program = [
	'fMain(x, y, z) = gAppend(gAppend(x, y), z);',
	'gAppend(Nil(), vs1) = vs1;',
	'gAppend(Cons(u, us), vs) = Cons(u, gAppend(us, vs));'
	];

var code = program.join('\n');

var fr = program[0].replace(/\s*/g, '');
var gr1 = program[1].replace(/\s*/g, '');
var gr2 = program[2].replace(/\s*/g, '');


var gc = 'gAppend(gAppend(x,y),z)';
var fc = 'f(a,f,f(a,x),A())'
	
var test1 = function() {
	return sll_parser.parse(code);
}