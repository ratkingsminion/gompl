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
	r",", _RESERVED, # separator
	r"\/\/[^\n]*", _IGNORE, # comments
	r"\+", _RESERVED, r"-", _RESERVED, r"\*", _RESERVED, r"/", _RESERVED, r"\%", _RESERVED,
	r"<=", _RESERVED, r"<", _RESERVED, r">=", _RESERVED, r">", _RESERVED,
	r"==", _RESERVED, r"!=", _RESERVED,
	r"\=", _RESERVED, r"\(", _RESERVED, r"\)", _RESERVED,
	r"[0-9]+\.[0-9]*", _FLOAT, r"[0-9]+", _INT,
	r"\"(.*?(?<!\\))\"", _STRING,
	r"[A-Za-z_][A-Za-z0-9_]*", _ID
]
const _TOKEN_AND: Array[String] = [ "and" ]
const _TOKEN_OR: Array[String] = [ "or" ]
const _TOKEN_EQUALITY: Array[String] = [ "==", "!=" ]
const _TOKEN_COMPARISON: Array[String] = [ "<=", "<", ">=", ">" ]
const _TOKEN_TERM: Array[String] = [ "-", "+" ]
const _TOKEN_FACTOR: Array[String] = [ "/", "*", "%" ]
const _TOKEN_UNARY: Array[String] = [ "not", "-" ]
const _TOKEN_ASSIGNMENT: Array[String] = [ "=" ]
const _TOKEN_KEYWORDS: Array[String] = [ "and", "or", "not", "if", "then", "else", "elif", "while", "do", "end", "stop", "skip", "interrupt" ]
const _TOKEN_UNDEFINED: Array[String] = [ "undefined" ]
const _TOKEN_BOOLS: Array[String] = [ "true", "false" ]

var debug_printing := false
var err: String
var target: Object

###

func _init(target_object: Object = null) -> void:
	self.target = target_object

###

## env is a Dictionary that contains all the variables assigned in the code
func eval(code: String, env = null, max_steps: int = 9223372036854775807, state = null):
	var tokens := tokenize_code(code)
	if not tokens: return null # some error happened
	var ast := parse_tokens(tokens)
	if not ast: return null # some error happened
	var instructions := compile(ast)
	if not instructions: return null # some error happened
	return run(instructions, env, max_steps, state)

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
	
## Step 3 - compile the AST to an arry of instructions, compiled via the AST
func compile(ast: Expr) -> Array[Array]:
	err = ""
	var it: Array[Array]
	ast.compile(self, it, [])
	if err: printerr(err); return []
	if debug_printing and it: print("INSTRUCTIONS: ", it)
	return it

## env is a Dictionary that contains all the variables assigned in the code
## If you don't provide env, a temporary one will be created
## If you provide a state Dictionary, it can be re-sed to continue the execution
func run(it: Array[Array], env = null, max_steps: int = 9223372036854775807, state = null):
	err = ""
	if env == null: env = {} if state == null else state.get("env", {})
	elif env is not Dictionary: _set_err("Environment must be a Dictionary"); env = {}
	
	var step: int = 0
	var stack: Array = [] if state == null else state.get("stack", [])
	var pos: int = 0 if state == null else state.get("pos", 0)
	
	var interrupted := false
	while not err and pos < it.size() and not interrupted:
		#print("run ", pos, ") ", it[pos], " - stack:", stack, " env:", env)
		match it[pos][0]:
			"undefined":
				stack.push_back(undefined)
			"bin_logic":
				var l = stack.pop_back()
				if it[pos][1] == "and":
					if not l or l is _Undefined: stack.push_back(false); pos = it[pos][2] - 1
				elif it[pos][1] == "or":
					if l and l is not _Undefined: stack.push_back(true); pos = it[pos][2] - 1
			"bin_logic_end":
				var r = stack.pop_back()
				stack.push_back(r or r is not _Undefined)
			"bin":
				var l = stack.pop_back()
				var r = stack.pop_back()
				if it[pos][1] == "==":
					stack.push_back(typeof(l) == typeof(r) and l == r)
				elif it[pos][1] == "!=":
					stack.push_back(typeof(l) != typeof(r) or l != r)
				elif l is _Undefined or r is _Undefined:
					_set_err("Can't use undefined variable in binary op")
					stack.push_back(Gompl.undefined)
				elif it[pos][1] in [ "+", "-", "*", "/", "%" ]:
					#print(">>> ", l, " ", it[pos][1], " ", r)
					match it[pos][1]:
						"+":
							if l is String or r is String: stack.push_back(str(l, r))
							else: stack.push_back(l + r)
						"-":
							if l is String: stack.push_back(l.replace(str(r), ""))
							elif r is String: _set_err("Incompatible types in binary op"); stack.push_back(Gompl.undefined)
							else: stack.push_back(l - r)
						"*":
							if l is String and (r is int or r is float): stack.push_back(l.repeat(r))
							elif (l is int or l is float) and r is String: stack.push_back(r.repeat(l))
							elif r is String and l is String: _set_err("Incompatible types in binary op"); stack.push_back(Gompl.undefined)
							else: stack.push_back(l * r)
						"/":
							if r is String or l is String: _set_err("Incompatible types in binary op"); stack.push_back(Gompl.undefined)
							elif r == 0: _set_err("Division by zero"); stack.push_back(Gompl.undefined)
							else: stack.push_back(l / r)
						"%":
							if r is String or l is String: _set_err("Incompatible types in binary op"); stack.push_back(Gompl.undefined)
							elif r == 0: _set_err("Division by zero"); stack.push_back(Gompl.undefined)
							else: stack.push_back(l % r)
				else:
					if (l is String and r is not String) or (r is String and l is not String):
						_set_err("Incompatible types for binary op"); stack.push_back(Gompl.undefined)
					else:
						match it[pos][1]:
							"<": stack.push_back(l < r)
							"<=": stack.push_back(l <= r)
							">": stack.push_back(l > r)
							">=": stack.push_back(l >= r)
			"unary":
				var r = stack.pop_back()
				match it[pos][1]:
					"not":
						if r is not bool: _set_err("Incompatible types for unary op"); stack.push_back(Gompl.undefined)
						else: stack.push_back(not r)
					"-":
						if r is not int and r is not float: _set_err("Incompatible types for unary op"); stack.push_back(Gompl.undefined)
						else: stack.push_back(-r)
			"assign":
				var res = stack.pop_back()
				env[it[pos][1]] = res
				stack.push_back(res)
			"literal":
				stack.push_back(it[pos][1])
			"pop":
				stack.pop_back()
			"id":
				stack.push_back(env.get(it[pos][1], undefined))
			"check":
				if stack.pop_back(): stack.pop_back()
				else: stack.push_back(undefined); pos = it[pos][1] - 1
			"jump":
				pos = it[pos][1] - 1
			"interrupt":
				interrupted = true
			"excall":
				var res
				if not it[pos][2]:
					res = target.call(it[pos][1])
				else:
					var args = []
					for i: int in it[pos][2]:
						var arg = stack.pop_back()
						args.append(arg if arg is not _Undefined else null)
					res = target.callv(it[pos][1], args)
				stack.push_back(res if res else undefined)
		
		pos += 1
		step += 1
		if step >= max_steps:
			interrupted = true
	
	if err: printerr(err); return null
	
	if interrupted and state != null:
		state["stack"] = stack
		state["pos"] = pos
		state["env"] = env
		state["steps"] = step
		return null
	
	if debug_printing and stack.back(): print("RESULT: ", stack.back())
	if debug_printing: print("ENVIRONMENT: ", env)
	return stack.back()

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
	func compile(_gompl: Gompl, it: Array[Array],_scope_stack: Array[Scope]) -> void: it.append([ "undefined" ])

class Scope:
	var start_pos: int
	var stops: Array[Array]
	func _init(p: int) -> void: start_pos = p

class Expr:
	func compile(_gompl: Gompl, _it: Array[Array],_scope_stack: Array[Scope]) -> void:
		pass
	
	# TODO make the operations more robust for different types
	class Binary extends Expr:
		var left: Expr
		var op: String
		var right: Expr
		func _init(l: Expr, o: String, r: Expr) -> void: left = l; op = o; right = r
		func _to_string() -> String: return str("Binary(", left, ", '", op, "', ", right, ")")
		func compile(gompl: Gompl, it: Array[Array],scope_stack: Array[Scope]) -> void:
			if op == "and" or op == "or":
				left.compile(gompl, it, scope_stack)
				var d = [ "bin_logic", op ]; it.append(d)
				right.compile(gompl, it, scope_stack)
				it.append([ "bin_logic_end" ])
				d.append(it.size())
			else:
				right.compile(gompl, it, scope_stack)
				left.compile(gompl, it, scope_stack)
				it.append([ "bin", op ])
	class Unary extends Expr:
		var op: String
		var right: Expr
		func _init(o: String, r: Expr) -> void: op = o; right = r
		func _to_string() -> String: return str("Unary('", op, "', ", right, ")")
		func compile(gompl: Gompl, it: Array[Array],scope_stack: Array[Scope]) -> void:
			right.compile(gompl, it, scope_stack)
			it.append([ "unary", op ])
	class Assignment extends Expr:
		var left: Identifier
		var op: String
		var right: Expr
		func _init(l: Identifier, o: String, r: Expr) -> void: left = l; op = o; right = r
		func _to_string() -> String: return str("Assignment(", left, ", '", op, "', ", right, ")")
		func compile(gompl: Gompl, it: Array[Array],scope_stack: Array[Scope]) -> void:
			right.compile(gompl, it, scope_stack)
			it.append([ "assign", left.name ])
	class Literal extends Expr:
		var lit
		func _init(l) -> void: lit = l
		func _to_string() -> String: return str("Literal(", lit, ", ", type_string(typeof(lit)), ")")
		func compile(_gompl: Gompl, it: Array[Array],_scope_stack: Array[Scope]) -> void:
			it.append([ "literal", lit ])
	class List extends Expr:
		var exprs: Array[Expr]
		func _init(a: Array[Expr]) -> void: exprs = a
		func _to_string() -> String: return str("List(", exprs.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array],scope_stack: Array[Scope]) -> void:
			for i: int in exprs.size():
				exprs[i].compile(gompl, it, scope_stack)
				if i != exprs.size() - 1: it.append([ "pop" ])
	class Identifier extends Expr:
		var name: String
		func _init(n: String) -> void: name = n
		func _to_string() -> String: return str("Identifier('", name, "')")
		func compile(_gompl: Gompl, it: Array[Array],_scope_stack: Array[Scope]) -> void:
			it.append([ "id", name ])
	class If extends Expr:
		var conds: Array[Expr]
		var bodies: Array[Expr]
		func _init(c: Array[Expr], b: Array[Expr]) -> void: conds = c; bodies = b
		func _to_string() -> String: return str("If(", conds, ", ", bodies.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array],scope_stack: Array[Scope]) -> void:
			var check: Array
			var jumps: Array[Array]
			for i: int in conds.size():
				if check: check.append(it.size())
				conds[i].compile(gompl, it, scope_stack)
				check = [ "check" ]; it.append(check)
				bodies[i].compile(gompl, it, scope_stack)
				jumps.append([ "jump" ]); it.append(jumps[-1])
			check.append(it.size())
			if bodies.size() > conds.size():
				bodies[-1].compile(gompl, it, scope_stack)
			for j in jumps:
				j.append(it.size())
	class While extends Expr:
		var cond: Expr
		var body: Expr
		func _init(c: Expr, b: Expr) -> void: cond = c; body = b
		func _to_string() -> String: return str("While(", cond, ", ", body, ")")
		func compile(gompl: Gompl, it: Array[Array],scope_stack: Array[Scope]) -> void:
			var start_pos := it.size()
			var scope := Scope.new(start_pos)
			scope_stack.push_back(scope)
			cond.compile(gompl, it, scope_stack)
			var check = [ "check" ]; it.append(check)
			body.compile(gompl, it, scope_stack)
			it.append([ "jump", start_pos])
			check.append(it.size())
			for s: Array in scope.stops: s.append(it.size()) # jump targets of stops
			scope_stack.erase(scope)
	class FlowControl extends Expr:
		var op: String
		func _init(o: String) -> void: op = o
		func _to_string() -> String: return str("Stop()")
		func compile(gompl: Gompl, it: Array[Array],scope_stack: Array[Scope]) -> void:
			if op == "interrupt":
				it.append([ "interrupt" ])
			elif not scope_stack:
				gompl._set_err(str("Unexpected '", op, "'"))
			else: 
				var jump = [ "jump" ]
				if op == "stop":
					it.append([ "undefined" ])
					scope_stack.back().stops.append(jump)
				elif op == "skip":
					jump.append(scope_stack.back().start_pos)
				it.append(jump)
	class FnCall extends Expr:
		var method: String
		var params: Array[Expr]
		func _init(m: String, p: Array[Expr]) -> void: method = m; params = p
		func _to_string() -> String: return str("FnCall('", method, "', ", params.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array],_scope_stack: Array[Scope]) -> void:
			if not gompl.target or not gompl.target.has_method(method):
				gompl._set_err(str("FnCall('", method, "', ", params.size(), "): Can not call function '", method, "'"))
				return
			if params.size() > gompl.target.get_method_argument_count(method):
				gompl._set_err(str("FnCall('", method, "', ", params.size(), "): Too many parameters for function '", method, "'"))
				return
			for i: int in range(params.size() -1, -1, -1):
				params[i].compile(gompl, it,_scope_stack)
			it.append([ "excall", method, params.size() ])

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
		var array: Array[Expr]
		while pos < tokens.size():
			var e := expression()
			if gompl.err: return null
			if not e: break
			array.append(e)
			expr = e
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
			_UNDEFINED: res = Gompl.undefined
			_BOOL: res = Expr.Literal.new(tokens[pos][0] == "true")
			_FLOAT: res = Expr.Literal.new(float(tokens[pos][0]))
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
					else: res = expr
				elif tokens[pos][0] == "if":
					var conds: Array[Expr]
					var bodies: Array[Expr]
					var expected := "if"
					while tokens[pos][0] == expected:
						pos += 1
						var cond := expression()
						if not cond: _set_err("Expect condition after '" + expected + "'"); break
						elif pos >= tokens.size(): _set_err("Expect 'then' after '" + expected + "' condition, early EOF"); break
						elif tokens[pos][0] != "then": _set_err("Expect 'then' after '" + expected + "' condition"); break
						conds.append(cond)
						pos += 1
						var body := expressions()
						if not body: _set_err("Expect body after 'then'"); break
						elif pos >= tokens.size(): _set_err("Expect 'elif', 'else' or 'end' after " + expected + "-body, early EOF"); break
						elif tokens[pos][0] != "else" and tokens[pos][0] != "elif" and tokens[pos][0] != "end": _set_err("Expect 'elif', 'else' or 'end' after " + expected + "-body"); break
						bodies.append(body)
						expected = "elif"
					if tokens[pos][0] == "else":
						pos += 1
						var body_else := expressions()
						if not body_else: _set_err("Expect body after 'else'")
						elif pos >= tokens.size() : _set_err("Expect 'end' after else-body, early EOF")
						elif tokens[pos][0] != "end": _set_err("Expect 'end' after else-body")
						else: bodies.append(body_else)
					if not gompl.err: res = Expr.If.new(conds, bodies)
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
						else: res = Expr.While.new(cond, body)
				elif tokens[pos][0] == "stop":
					res = Expr.FlowControl.new("stop")
				elif tokens[pos][0] == "skip":
					res = Expr.FlowControl.new("skip")
				elif tokens[pos][0] == "interrupt":
					res = Expr.FlowControl.new("interrupt")
				else:
					pass # do nothing, otherwise expressions() always spits out an error
		if res: pos += 1
		return res

	func parse() -> Expr:
		var res := expressions()
		if gompl.err: return null
		return res
