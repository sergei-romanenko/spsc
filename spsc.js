
function remove_ws() {
	var code = document.getElementById('code').value;
	var code2 = code.replace(/\s*/g, '');
	alert(code2);
}

var P = Parser;
var g = {
	v_name: P.token(/^[a-z]\w*/),
	c_name: P.token(/^[A-Z]\w*/),
	g_name: P.token(/^g\w*/),
	f_name: P.token(/^f\w*/),
	lparen: P.token(/^\(/),
	rparen: P.token(/^\)/),
	comma:  P.token(/^,/),
	empty:  P.token('')
};

g.pattern = P.process(P.and([g.c_name, g.lparen, P.repeat0_del(g.v_name, g.comma), g.rparen]),
				function(result) {return {'type': 'PAT', 'name': result[0],'args': result[2]};});
g.f_lhs = P.process(P.and([g.f_name, g.lparen, P.repeat1_del(g.v_name, g.comma), g.rparen]),
				function(result) {return {'type': 'FLHS', 'name': result[0], 'args': result[2]};});
g.g_lhs = P.process(P.and([g.g_name, g.lparen, g.pattern, 
                                     P.or([
                                           	P.process(P.and([g.comma, P.repeat1_del(g.v_name, g.comma)]), function(r){return r[1];}), 
                                           	P.process(g.empty, function(r){return[];})
                                           ]), 
                                     g.rparen]),
				function(result) {return {'type': 'GLHS', 'name': result[0], 'pattern': result[2], 'args': result[3]};});

var p = {};

(function() {
var o = (p.rules = {
	vrb: function(t) {return P.process(g.v_name, function(v){return {'type': 'VRB', 'name': v};})(t);},
	ctr: function(t) {
		return P.process(P.and([g.c_name, g.lparen, P.repeat0_del(o.term, g.comma), g.rparen]),
					function(result) {return {'type': 'CTR', 'name': result[0], 'args': result[2]};})(t);
	},
	term: function(t) {
		return P.or([o.ctr, o.vrb])(t);
	}
	});
})();
