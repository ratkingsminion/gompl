class_name Gompl
extends RefCounted

## A simple interpreted scripting language for Godot
## Based on https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-
## and https://jayconrod.com/posts/65/how-to-build-a-parser-by-hand
## and https://craftinginterpreters.com/parsing-expressions.html

static var undefined: _Undefined = _Undefined.new() # only use this
	
const _IGNORE := "IGN"
const _RESERVED := "RSV"
const _INT := "INT"
const _FLOAT := "FLT"
const _STRING := "STR"
const _UNDEFINED := "NIL"
const _BOOL := "BOOL"
const _ID := "ID"
const _TOKEN_EXPRESSIONS: Array[String] = [
	r"[ \n\t]+", _IGNORE, r"#[^\n]*", _IGNORE, # whitespaces
	r",", _RESERVED, r",", _RESERVED, # separator
	r"\/\/[^\n]*", _IGNORE, # comments
	r"\+", _RESERVED, r"-", _RESERVED, r"\*", _RESERVED, r"/", _RESERVED, r"%", _RESERVED,
	r"<=", _RESERVED, r"<", _RESERVED, r">=", _RESERVED, r">", _RESERVED,
	r"==", _RESERVED, r"!=", _RESERVED,
	r"\=", _RESERVED, r"\(", _RESERVED, r"\)", _RESERVED,
	r"[0-9]+\.[0-9]*", _FLOAT,
	r"[0-9]+", _INT,
	r"\"(.*?(?<!\\))\"", _STRING,
	r"[A-Za-z_][A-Za-z0-9_]*", _ID
]
const _TOKEN_AND: Array[String] = [ "and" ]
const _TOKEN_OR: Array[String] = [ "or" ]
const _TOKEN_EQUALITY: Array[String] = [ "==", "!=" ]
const _TOKEN_COMPARISON: Array[String] = [ "<=", "<", ">=", ">" ]
const _TOKEN_TERM: Array[String] = [ "-", "+" ]
const _TOKEN_FACTOR: Array[String] = [ "/", "*" ]
const _TOKEN_UNARY: Array[String] = [ "not", "-" ]
const _TOKEN_ASSIGNMENT: Array[String] = [ "=" ]
const _TOKEN_KEYWORDS: Array[String] = [ "and", "or", "not", "if", "then", "else", "elif", "while", "do", "end", "stop", "skip" ]
const _TOKEN_UNDEFINED: Array[String] = [ "undefined" ]
const _TOKEN_BOOLS: Array[String] = [ "true", "false" ]

var debug_printing := false
var err: String
var target: Object

var _scope_stack: Array[Scope]
var _call_stack: Array[Expr]
var _call_count := 0

###

func _init(target_object: Object = null) -> void:
	self.target = target_object

###

## env is a Dictionary that contains all the variables assigned in the code
func eval(code: String, env = null):
	var tokens := tokenize_code(code)
	if tokens:
		var ast := parse_tokens(tokens)
		if ast: return evaluate_ast(ast, env)
	return null # some error happened

###

## Step 1 - returns the tokens of the code
func tokenize_code(code: String) -> Array[Array]:
	err = ""
	var tokens := _lex(code)
	if err: printerr(err); return []
	if debug_printing and tokens: print("TOKENS: ", tokens)
	return tokens

## Step 2 - returns the AST (abstract syntax tree) of the tokens
func parse_tokens(tokens: Array[Array]) -> Expr:
	err = ""
	var parser := Parser.new(self, tokens)
	if err: printerr(err); return null
	var ast := parser.parse()
	if err: printerr(err); return null
	if debug_printing and ast: print("AST: ", ast)
	return ast

## Step 3 - evaluate the AST
## env is a Dictionary that contains all the variables assigned in the code
## If you don't provide env, a temporary one will be created
func evaluate_ast(ast: Expr, env = null):
	err = ""
	if env == null: env = {}
	elif env is not Dictionary: _set_err("Environment must be a Dictionary"); env = {}
	_call_stack.clear()
	_call_count = 0
	var res = ast.eval(self, env)
	_scope_stack.clear()
	if err: printerr(err); return null
	if debug_printing and res: print("RESULT: ", res)
	if debug_printing: print("ENVIRONMENT: ", env)
	return res
	

###

func _set_err(e: String, overwrite := false) -> void:
	if err and not overwrite: return
	err = str("Gompl: ", e)

### LEXER

func _lex(code: String) -> Array[Array]:
	var pos := 0
	var tokens: Array[Array] = []
	var reg := RegEx.new()
	while pos < code.length():
		var res: RegExMatch = null
		var tag: String
		for tidx: int in range(0, _TOKEN_EXPRESSIONS.size(), 2):
			reg.compile(_TOKEN_EXPRESSIONS[tidx])
			res = reg.search(code, pos)
			if res and res.get_start() == pos:
				tag = _TOKEN_EXPRESSIONS[tidx + 1]
				if tag == _IGNORE: break
				var value := res.get_string()
				if tag == _ID:
					if value in _TOKEN_KEYWORDS: tag = _RESERVED
					elif value in _TOKEN_BOOLS: tag = _BOOL
					elif value in _TOKEN_UNDEFINED: tag = _UNDEFINED
				var token: Array[String] = [ value, tag ]
				tokens.append(token)
				break
			else:
				res = null
		if res: pos = res.get_end()
		else: _set_err(str("Illegal character: ", code[pos], " at token ", pos)); return []
	return tokens

### EXPRESSIONS

class _Undefined extends Expr:
	func _init() -> void: pass
	func _to_string() -> String: return "undefined"
	func eval(_gompl: Gompl, _env: Dictionary): return Gompl.undefined

class Scope:
	var scope: Expr
	var last: Expr = null
	func _init(s: Expr) -> void: scope = s

class Expr:
	var _list: Array[Expr]
	var _idx: int
	
	func _init(p: Parser) -> void:
		_list = p.exprs
		_idx = _list.size()
		_list.append(self)
	
	func eval(_gompl: Gompl, _env: Dictionary):
		_gompl._call_count += 1
		_gompl._call_stack.push_back(self)
		#print("call ", _gompl._call_count, ") ", _idx, " s:", _gompl._call_stack.size())
		var res = _eval(_gompl, _env)
		_gompl._call_stack.pop_back()
		return res
	
	func _eval(_gompl: Gompl, _env: Dictionary):
		return null
	
	# TODO make the operations more robust for different types
	class Binary extends Expr:
		var left: int
		var op: String
		var right: int
		func _init(parser: Parser, l: int, o: String, r: int) -> void: super(parser); left = l; op = o; right = r
		func _to_string() -> String: return str("Binary(", _list[left], ", '", op, "', ", _list[right], ")")
		func _eval(gompl: Gompl, env: Dictionary):
			var l = _list[left].eval(gompl, env)
			var r
			if op == "and":
				if not l or l is _Undefined: return false
				r = _list[right].eval(gompl, env)
				return r and r is not _Undefined
			elif op == "or":
				if l and l is not _Undefined: return true
				r = _list[right].eval(gompl, env)
				return r and r is not _Undefined
			r = _list[right].eval(gompl, env)
			match op:
				"==": return typeof(l) == typeof(r) and l == r
				"!=": return typeof(l) != typeof(r) or l != r
			if l is _Undefined: gompl._set_err("Trying to use undefined variable in binary operation"); return Gompl.undefined
			if r is _Undefined: gompl._set_err("Trying to use undefined variable in binary operation"); return Gompl.undefined
			match op:
				"<": return l < r
				"<=": return l <= r
				">": return l > r
				">=": return l >= r
				"+":
					if l is String or r is String: return str(l, r)
					return l + r
				"-":
					if l is String and r is String: return l.replace(r, "")
					return l - r
				"*":
					if l is String and (r is int or r is float): return l.repeat(r)
					elif (l is int or l is float) and r is String: return r.repeat(l)
					return l * r
				"/":
					if r == 0: gompl._set_err("Division by zero"); return Gompl.undefined
					return l / r
				"%":
					if r == 0: gompl._set_err("Division by zero"); return Gompl.undefined
					return l % r
	class Unary extends Expr:
		var op: String
		var right: int
		func _init(parser: Parser, o: String, r: int) -> void: super(parser); op = o; right = r
		func _to_string() -> String: return str("Unary('", op, "', ", _list[right], ")")
		func _eval(gompl: Gompl, env: Dictionary):
			var r = _list[right].eval(gompl, env)
			if op == "not": return not r
			if r is _Undefined: gompl._set_err("Trying to use undefined variable in unary operation"); return Gompl.undefined
			if op == "-": return -r
	class Assignment extends Expr:
		var left: Identifier
		var op: String
		var right: int
		func _init(parser: Parser, l: Identifier, o: String, r: int) -> void: super(parser); left = l; op = o; right = r
		func _to_string() -> String: return str("Assignment(", left, ", '", op, "', ", _list[right], ")")
		func _eval(gompl: Gompl, env: Dictionary):
			var res = _list[right].eval(gompl, env)
			if res is _Undefined: env.erase(left.name)
			else: env[left.name] = res
			return res
	class Literal extends Expr:
		var lit
		func _init(parser: Parser, l) -> void: super(parser); lit = l
		func _to_string() -> String: return str("Literal(", lit, ", ", type_string(typeof(lit)), ")")
		func _eval(_gompl: Gompl, _env: Dictionary):
			return lit
	class List extends Expr:
		var exprs: Array[int]
		func _init(parser: Parser, a: Array[int]) -> void: super(parser); exprs = a
		func _to_string() -> String: return str("List(", exprs.map(func(i): return _list[i]), ")")
		func _eval(gompl: Gompl, env: Dictionary):
			var res
			if gompl._scope_stack:
				for e: int in exprs:
					var s: Expr = gompl._scope_stack.back().last
					if s is FlowControl: break
					res = _list[e].eval(gompl, env)
				return res
			else:
				for e: int in exprs:
					res = _list[e].eval(gompl, env)
			return res
	class Identifier extends Expr:
		var name: String
		func _init(parser: Parser, n: String) -> void: super(parser); name = n
		func _to_string() -> String: return str("Identifier('", name, "')")
		func _eval(_gompl: Gompl, env: Dictionary):
			return env.get(name, Gompl.undefined)
	class If extends Expr:
		var conds: Array[int]
		var exprs: Array[int]
		func _init(parser: Parser, c: Array[int], e: Array[int]) -> void: super(parser); conds = c; exprs = e
		func _to_string() -> String: return str("If(", conds, ", ", exprs.map(func(i): return _list[i]), ")")
		func _eval(gompl: Gompl, env: Dictionary):
			var i := 0
			while i < conds.size():
				var c = _list[conds[i]].eval(gompl, env)
				if c and c is not _Undefined: return _list[exprs[i]].eval(gompl, env)
				i = i + 1
			return _list[exprs[i]].eval(gompl, env) if i < exprs.size() else Gompl.undefined
	class While extends Expr:
		var cond: int
		var expr: int
		func _init(parser: Parser, c: int, e: int) -> void: super(parser); cond = c; expr = e
		func _to_string() -> String: return str("While(", cond, ", ", expr, ")")
		func _eval(gompl: Gompl, env: Dictionary):
			var res
			var scope := Scope.new(self)
			gompl._scope_stack.push_back(scope)
			while _list[cond].eval(gompl, env):
				res = _list[expr].eval(gompl, env)
				if scope.last is FlowControl and scope.last.op == "stop": scope.last = null; break
				scope.last = null
			gompl._scope_stack.erase(scope)
			return res
	class FlowControl extends Expr:
		var op: String
		func _init(parser: Parser, o: String) -> void: super(parser); op = o
		func _to_string() -> String: return str("Stop()")
		func _eval(gompl: Gompl, _env: Dictionary):
			if gompl._scope_stack: gompl._scope_stack.back().last = self
			else: gompl._set_err(str("Unexpected '", op, "'"))
			return Gompl.undefined
	class FnCall extends Expr:
		var method: String
		var params: Array[int]
		func _init(parser: Parser, m: String, p: Array[int]) -> void: super(parser); method = m; params = p
		func _to_string() -> String: return str("FnCall('", method, "', ", params.map(func(i): return _list[i]), ")")
		func _eval(gompl: Gompl, env: Dictionary):
			if not gompl.target or not gompl.target.has_method(method):
				gompl._set_err(str("FnCall('", method, "', ", params.size(), "): Could not call function '", method, "'"))
				return null
			if params: return gompl.target.callv(method, params.map(func(p: int): return _list[p].eval(gompl, env) if p != -1 and _list[p] is not _Undefined else null))
			return gompl.target.call(method)

### PARSER

class Parser:
	var gompl: Gompl
	var tokens: Array[Array]
	var exprs: Array[Expr]
	var pos := 0
	
	func _init(g: Gompl, t: Array[Array]) -> void:
		gompl = g
		tokens = t
	
	func _set_err(e: String) -> void:
		var error := str("PARSER[", pos, "] ", e)
		gompl._set_err(error, false)
	
	func expressions() -> Expr:
		var expr: Expr = null
		var array: Array[int]
		while pos < tokens.size():
			var e := expression()
			if gompl.err: return null
			if not e: break
			expr = e
			array.append(e._idx)
		return Expr.List.new(self, array) if array.size() > 1 else expr

	func expression() -> Expr:
		return assignment()
	
	func assignment() -> Expr:
		var expr := op_and()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_ASSIGNMENT:
			if expr is not Expr.Identifier: expr = null; break
			var operator: String = tokens[pos][0]
			pos += 1
			var right := expression()
			expr = Expr.Assignment.new(self, expr as Expr.Identifier, operator, right._idx)
		return expr
	
	func op_and() -> Expr:
		var expr := op_or()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_AND:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := op_or()
			expr = Expr.Binary.new(self, expr._idx, operator, right._idx)
		return expr
	
	func op_or() -> Expr:
		var expr := equality()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_OR:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := equality()
			expr = Expr.Binary.new(self, expr._idx, operator, right._idx)
		return expr
	
	func equality() -> Expr:
		var expr := comparison()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_EQUALITY:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := comparison()
			expr = Expr.Binary.new(self, expr._idx, operator, right._idx)
		return expr
	
	func comparison() -> Expr:
		var expr := term()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_COMPARISON:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := term()
			expr = Expr.Binary.new(self, expr._idx, operator, right._idx)
		return expr
	
	func term() -> Expr:
		var expr := factor()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_TERM:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := factor()
			expr = Expr.Binary.new(self, expr._idx, operator, right._idx)
		return expr
	
	func factor() -> Expr:
		var expr := unary()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_FACTOR:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := unary()
			expr = Expr.Binary.new(self, expr._idx, operator, right._idx)
		return expr
	
	func unary() -> Expr:
		if pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_UNARY:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := unary()
			return Expr.Unary.new(self, operator, right._idx)
		return primary()

	func primary() -> Expr:
		if pos >= tokens.size(): return null
		var res: Expr = null
		match tokens[pos][1]:
			_UNDEFINED: res = Gompl.undefined
			_BOOL: res = Expr.Literal.new(self, tokens[pos][0] == "true")
			_FLOAT: res = Expr.Literal.new(self, float(tokens[pos][0]))
			_INT: res = Expr.Literal.new(self, int(tokens[pos][0]))
			_STRING: res = Expr.Literal.new(self, tokens[pos][0].substr(1, tokens[pos][0].length() - 2).c_unescape()) # removing the quotation marks
			_ID:
				var ident = tokens[pos][0]
				if pos < tokens.size() - 1 and tokens[pos + 1][0] == "(":
					pos += 2
					var params: Array[int] = []
					while pos < tokens.size() and tokens[pos][0] != ")":
						var expr := expressions()
						if not expr: _set_err("Expect expression inside params list"); break
						params.append(expr._idx)
						if pos >= tokens.size(): _set_err("Expect ',' or ')' in params list, early EOF"); break
						elif tokens[pos][0] == ",": pos += 1; continue
						elif tokens[pos][0] != ")": _set_err("Expect ',' or ')' in params list"); break
					if not gompl.err:
						res = Expr.FnCall.new(self, ident, params)
				else:
					res = Expr.Identifier.new(self, ident)
			_RESERVED:
				if tokens[pos][0] == "(":
					pos += 1
					var expr := expression()
					if not expr: _set_err("Expect expression inside group")
					elif pos >= tokens.size(): _set_err("Expect ')' after expression, early EOF")
					elif tokens[pos][0] != ")": _set_err("Expect ')' after expression")
					else: res = expr
				elif tokens[pos][0] == "if":
					var conds: Array[int]
					var bodies: Array[int]
					var expected := "if"
					while tokens[pos][0] == expected:
						pos += 1
						var cond := expression()
						if not cond: _set_err("Expect condition after '" + expected + "'"); break
						elif pos >= tokens.size(): _set_err("Expect 'then' after '" + expected + "' condition, early EOF"); break
						elif tokens[pos][0] != "then": _set_err("Expect 'then' after '" + expected + "' condition"); break
						conds.append(cond._idx)
						pos += 1
						var body := expressions()
						if not body: _set_err("Expect body after 'then'"); break
						elif pos >= tokens.size(): _set_err("Expect 'elif', 'else' or 'end' after " + expected + "-body, early EOF"); break
						elif tokens[pos][0] != "else" and tokens[pos][0] != "elif" and tokens[pos][0] != "end": _set_err("Expect 'elif', 'else' or 'end' after " + expected + "-body"); break
						bodies.append(body._idx)
						expected = "elif"
					if tokens[pos][0] == "else":
						pos += 1
						var body_else := expressions()
						if not body_else: _set_err("Expect body after 'else'")
						elif pos >= tokens.size() : _set_err("Expect 'end' after else-body, early EOF")
						elif tokens[pos][0] != "end": _set_err("Expect 'end' after else-body")
						else: bodies.append(body_else._idx)
					if not gompl.err: res = Expr.If.new(self, conds, bodies)
				elif tokens[pos][0] == "while":
					pos += 1
					var cond := expression()
					if not cond: _set_err("Expect condition after 'while'")
					elif pos >= tokens.size(): _set_err("Expect 'do' after 'while' condition, early EOF")
					elif tokens[pos][0] != "do": _set_err("Expect 'do' after 'while' condition")
					else:
						pos += 1
						var body := expressions()
						if not body: _set_err("Expect body after 'do'")
						elif pos >= tokens.size() : _set_err("Expect 'end' after while-body, early EOF")
						elif tokens[pos][0] != "end": _set_err("Expect 'end' after while-body")
						else: res = Expr.While.new(self, cond._idx, body._idx)
				elif tokens[pos][0] == "stop":
					res = Expr.FlowControl.new(self, "stop")
				elif tokens[pos][0] == "skip":
					res = Expr.FlowControl.new(self, "skip")
				else:
					pass # do nothing, otherwise expressions() always spits out an error
		if res: pos += 1
		return res

	func parse() -> Expr:
		var res := expressions()
		if gompl.err: return null
		return res
