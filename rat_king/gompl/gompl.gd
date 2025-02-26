class_name Gompl
extends RefCounted

## A simple interpreted scripting language for Godot
## Based on https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-
## and https://jayconrod.com/posts/65/how-to-build-a-parser-by-hand

const _IGNORE := "IGN"
const _RESERVED := "RSV"
const _INT := "INT"
const _STRING := "STR"
const _BOOL := "BOOL"
const _ID := "ID"
const _TOKEN_EXPRESSIONS: Array[String] = [
	r"[ \n\t]+", _IGNORE, r"#[^\n]*", _IGNORE, # whitespaces
	r",", _RESERVED, r",", _RESERVED, # separator
	r"\/\/[^\n]*", _IGNORE, # comments
	r"\+", _RESERVED, r"-", _RESERVED, r"\*", _RESERVED, r"/", _RESERVED,
	r"<=", _RESERVED, r"<", _RESERVED, r">=", _RESERVED, r">", _RESERVED,
	r"==", _RESERVED, r"!=", _RESERVED,
	r"\=", _RESERVED, r"\(", _RESERVED, r"\)", _RESERVED,
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
const _TOKEN_KEYWORDS: Array[String] = [ "and", "or", "not", "if", "then", "else", "while", "do", "end" ]
const _TOKEN_BOOLS: Array[String] = [ "true", "false" ]

var err: String
var target: Object

###

func _init(target_object: Object = null) -> void:
	self.target = target_object

###

## env is a Dictionary that contains all the variables assigned in the code
func eval(code: String, env = null):
	err = ""
	var tokens := _lex(code)
	if err: printerr(err); return null
	if not tokens: return null
	#print("TOKENS: ", tokens)
	var parser := Parser.new(self, tokens)
	if err: printerr(err); return null
	var ast := parser.parse()
	if err: printerr(err); return null
	#if ast: print("AST: ", ast)
	if env == null: env = {}
	elif env is not Dictionary: _set_err("Environment must be a Dictionary"); env = {}
	var res = ast.eval(self, env)
	if err: printerr(err); return null
	#if res: print("RESULT: ", res)
	#print("ENVIRONMENT: ", env)
	return res

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
				if tag == _ID and value in _TOKEN_KEYWORDS: tag = _RESERVED
				if tag == _ID and value in _TOKEN_BOOLS: tag = _BOOL
				var token: Array[String] = [ value, tag ]
				tokens.append(token)
				break
			else:
				res = null
		if res: pos = res.get_end()
		else: _set_err(str("Illegal character: ", code[pos], " at token ", pos)); return []
	return tokens

### EXPRESSIONS

class Expr:
	func eval(_gompl: Gompl, _env: Dictionary):
		return null
	
	# TODO make the operations more robust for different types
	class Binary extends Expr:
		var left: Expr
		var op: String
		var right: Expr
		func _init(l: Expr, o: String, r: Expr) -> void: left = l; op = o; right = r
		func _to_string() -> String: return str("Binary(", left, ", '", op, "', ", right, ")")
		func eval(gompl: Gompl, env: Dictionary):
			var l = left.eval(gompl, env)
			var r = right.eval(gompl, env)
			match op:
				"and": return l and r
				"or": return l or r
				"==": return l == r
				"!=": return l != r
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
					if l is String and r is int: return l.repeat(r)
					return l * r
				"/": return l / r
	class Unary extends Expr:
		var op: String
		var right: Expr
		func _init(o: String, r: Expr) -> void: op = o; right = r
		func _to_string() -> String: return str("Unary('", op, "', ", right, ")")
		func eval(gompl: Gompl, env: Dictionary):
			match op:
				"not": return not right.eval(gompl, env)
				"-": return -right.eval(gompl, env)
	class Assignment extends Expr:
		var left: Identifier
		var op: String
		var right: Expr
		func _init(l: Identifier, o: String, r: Expr) -> void: left = l; op = o; right = r
		func _to_string() -> String: return str("Assignment(", left, ", '", op, "', ", right, ")")
		func eval(gompl: Gompl, env: Dictionary):
			var res = right.eval(gompl, env)
			env[left.name] = res
			return res
	class Literal extends Expr:
		var lit
		func _init(l) -> void: lit = l
		func _to_string() -> String: return str("Literal(", lit, ", ", type_string(typeof(lit)), ")")
		func eval(_gompl: Gompl, _env: Dictionary):
			return lit
	class Grouping extends Expr:
		var expr: Expr
		func _init(e: Expr) -> void: expr = e
		func _to_string() -> String: return str("Grouping(", expr, ")")
		func eval(gompl: Gompl, env: Dictionary):
			return expr.eval(gompl, env)
	class List extends Expr:
		var exprs: Array[Expr]
		func _init(a: Array[Expr]) -> void: exprs = a
		func _to_string() -> String: return str("List(", exprs, ")")
		func eval(gompl: Gompl, env: Dictionary):
			var res
			for e: Expr in exprs:
				res = e.eval(gompl, env)
			return res
	class Identifier extends Expr:
		var name: String
		func _init(n: String) -> void: name = n
		func _to_string() -> String: return str("Identifier('", name, "')")
		func eval(_gompl: Gompl, env: Dictionary):
			return env[name] if env.has(name) else 0 # TODO: standard value 0
	class If extends Expr:
		var cond: Expr
		var true_expr: Expr
		var false_expr: Expr
		func _init(c: Expr, t: Expr, f: Expr) -> void: cond = c; true_expr = t; false_expr = f
		func _to_string() -> String: return str("If(", cond, ", ", true_expr, ", ", false_expr, ")")
		func eval(gompl: Gompl, env: Dictionary):
			if cond.eval(gompl, env): return true_expr.eval(gompl, env)
			return false_expr.eval(gompl, env) if false_expr else null
	class While extends Expr:
		var cond: Expr
		var expr: Expr
		func _init(c: Expr, e: Expr) -> void: cond = c; expr = e
		func _to_string() -> String: return str("While(", cond, ", ", expr, ")")
		func eval(gompl: Gompl, env: Dictionary):
			var res
			while cond.eval(gompl, env):
				res = expr.eval(gompl, env)
			return res
	class FnCall extends Expr:
		var method: String
		var params: Array[Expr]
		func _init(m: String, p: Array[Expr]) -> void: method = m; params = p
		func _to_string() -> String: return str("FnCall('", method, "', ", params, ")")
		func eval(gompl: Gompl, env: Dictionary):
			if not gompl.target or not gompl.target.has_method(method):
				gompl._set_err(str("FnCall('", method, "', ", params.size(), "): Could not call function '", method, "'"))
				return null
			if params: return gompl.target.callv(method, params.map(func(p): return p.eval(gompl, env) if p else null))
			return gompl.target.call(method)

### PARSER

class Parser:
	var gompl: Gompl
	var tokens: Array[Array]
	var pos := 0
	
	func _init(g: Gompl, t: Array[Array]) -> void:
		gompl = g
		tokens = t
	
	func _set_err(e: String) -> void:
		var error := str("PARSER[", pos, "] ", e)
		gompl._set_err(error, false)
	
	func expressions() -> Expr:
		var expr: Expr = null
		var array: Array[Expr]
		while pos < tokens.size():
			var e := expression()
			if gompl.err: return null
			if not e: break
			expr = e
			array.append(e)
		return Expr.List.new(array) if array.size() > 1 else expr

	func expression() -> Expr:
		return assignment()
	
	func assignment() -> Expr:
		var expr := op_and()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_ASSIGNMENT:
			if expr is not Expr.Identifier: expr = null; break
			var operator: String = tokens[pos][0]
			pos += 1
			var right := expression()
			expr = Expr.Assignment.new(expr as Expr.Identifier, operator, right)
		return expr
	
	func op_and() -> Expr:
		var expr := op_or()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_AND:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := op_or()
			expr = Expr.Binary.new(expr, operator, right)
		return expr
	
	func op_or() -> Expr:
		var expr := equality()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_OR:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := equality()
			expr = Expr.Binary.new(expr, operator, right)
		return expr
	
	func equality() -> Expr:
		var expr := comparison()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_EQUALITY:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := comparison()
			expr = Expr.Binary.new(expr, operator, right)
		return expr
	
	func comparison() -> Expr:
		var expr := term()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_COMPARISON:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := term()
			expr = Expr.Binary.new(expr, operator, right)
		return expr
	
	func term() -> Expr:
		var expr := factor()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_TERM:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := factor()
			expr = Expr.Binary.new(expr, operator, right)
		return expr
	
	func factor() -> Expr:
		var expr := unary()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_FACTOR:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := unary()
			expr = Expr.Binary.new(expr, operator, right)
		return expr
	
	func unary() -> Expr:
		if pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_UNARY:
			var operator: String = tokens[pos][0]
			pos += 1
			var right := unary()
			return Expr.Unary.new(operator, right)
		return primary()

	func primary() -> Expr:
		if pos >= tokens.size(): return null
		var res: Expr = null
		match tokens[pos][1]:
			_BOOL: res = Expr.Literal.new(tokens[pos][0] == "true")
			_INT: res = Expr.Literal.new(int(tokens[pos][0]))
			_STRING: res = Expr.Literal.new(tokens[pos][0].substr(1, tokens[pos][0].length() - 2).c_unescape()) # removing the quotation marks
			_ID:
				var ident = tokens[pos][0]
				if pos < tokens.size() - 1 and tokens[pos + 1][0] == "(":
					pos += 2
					var params: Array[Expr] = []
					while pos < tokens.size() and tokens[pos][0] != ")":
						var expr := expressions()
						if not expr: _set_err("Expect expression inside params list"); break
						params.append(expr)
						if pos >= tokens.size(): _set_err("Expect ',' or ')' in params list, early EOF"); break
						elif tokens[pos][0] == ",": pos += 1; continue
						elif tokens[pos][0] != ")": _set_err("Expect ',' or ')' in params list"); break
					if not gompl.err:
						res = Expr.FnCall.new(ident, params)
				else:
					res = Expr.Identifier.new(ident)
			_RESERVED:
				if tokens[pos][0] == "(":
					pos += 1
					var expr := expression()
					if not expr: _set_err("Expect expression inside group")
					elif pos >= tokens.size(): _set_err("Expect ')' after expression, early EOF")
					elif tokens[pos][0] != ")": _set_err("Expect ')' after expression")
					else: res = Expr.Grouping.new(expr)
				elif tokens[pos][0] == "if":
					pos += 1
					var cond := expression()
					if not cond: _set_err("Expect condition after 'if'")
					elif pos >= tokens.size(): _set_err("Expect 'then' after condition, early EOF")
					elif tokens[pos][0] != "then": _set_err("Expect 'then' after condition")
					else:
						pos += 1
						var body_true := expressions()
						if not body_true: _set_err("Expect body after 'then'")
						elif pos >= tokens.size(): _set_err("Expect 'end' or 'else' after if-body, early EOF")
						elif tokens[pos][0] != "else" and tokens[pos][0] != "end": _set_err("Expect 'end' or 'else' after if-body")
						elif tokens[pos][0] == "end": res = Expr.If.new(cond, body_true, null)
						elif tokens[pos][0] == "else":
							pos += 1
							var body_false := expressions()
							if not body_false: _set_err("Expect body after 'else'")
							elif pos >= tokens.size() : _set_err("Expect 'end' after if-body, early EOF")
							elif tokens[pos][0] != "end": _set_err("Expect 'end' after if-body")
							else: res = Expr.If.new(cond, body_true, body_false)
				elif tokens[pos][0] == "while":
					pos += 1
					var cond := expression()
					if not cond: _set_err("Expect condition after 'while'")
					elif pos >= tokens.size(): _set_err("Expect 'do' after condition, early EOF")
					elif tokens[pos][0] != "do": _set_err("Expect 'do' after condition")
					else:
						pos += 1
						var body := expressions()
						if not body: _set_err("Expect body after 'do'")
						elif pos >= tokens.size() : _set_err("Expect 'end' after while-body, early EOF")
						elif tokens[pos][0] != "end": _set_err("Expect 'end' after while-body")
						else: res = Expr.While.new(cond, body)
				else:
					pass # do nothing, otherwise expressions() always spits out an error
		if res: pos += 1
		return res

	func parse() -> Expr:
		var res := expressions()
		if gompl.err: return null
		return res
