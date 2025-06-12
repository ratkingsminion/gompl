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

const T_ANY = "any"
const T_NUMBER = "number"
const T_STRING = "string"
const T_BOOL = "bool"
const T_UNDEFINED = "undefined"
var _registered_funcs: Dictionary

###

func _init(target_object: Object = null) -> void:
	self.target = target_object

###

## param_types is an array filled with types (T_ANY, T_INT, etc.)
## optional_params is the amount of optional parameters of the function
func register_func(func_name: String, callable: Callable, param_types: Array[String] = [], optional_params := 0) -> void:
	if not func_name or not callable: printerr("Invalid function registration"); return
	if _registered_funcs.has(func_name): printerr("Function name already registered"); return
	_registered_funcs[func_name] = [ callable, param_types, optional_params ]

func unregister_func(func_name: String) -> void:
	_registered_funcs.erase(func_name)

## env is a Dictionary that contains all the variables assigned in the code
func eval(code: String, env = null, state = null, max_steps: int = 9223372036854775800):
	var tokens := tokenize_code(code)
	if not tokens: return null # some error happened
	var ast := parse_tokens(tokens)
	if not ast: return null # some error happened
	var instructions := compile(ast)
	if not instructions: return null # some error happened
	return run(instructions, env, state, max_steps)

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
	if debug_printing: print("AST: ", ast)
	return ast
	
## Step 3 - compile the AST to an arry of instructions, compiled via the AST
func compile(ast: Expr) -> Array[Array]:
	err = ""
	var it: Array[Array]
	ast.compile(self, it, [])
	if err: printerr(err); return []
	if debug_printing and it: print("INSTRUCTIONS: ", it)
	return it

## Step 4 - iterate over the array of instructions, using it as a lower level language
## env is a Dictionary that contains all the variables assigned in the code
## If you don't provide env, a temporary one will be created
## If you provide a state Dictionary, it can be re-used to continue the execution after it was interrupted
func run(it: Array[Array], env = null, state = null, max_steps: int = 9223372036854775800):
	err = ""
	if env == null: env = {} if state is not Dictionary else state.get("env", {})
	elif env is not Dictionary: _set_err("Environment must be a Dictionary"); env = {}
	
	var step: int = 0
	var stack: Array = [] if state is not Dictionary else state.get("stack", [])
	var pos: int = 0 if state is not Dictionary else state.get("pos", 0)
	
	while not err and pos < it.size():
		if (state is StringName and state == &"interrupted") or (state is Dictionary and state.has("interrupted")):
			break
		#print("run ", pos, ") ", it[pos], " - stack:", stack, " env:", env)
		match it[pos][1]:
			"undefined":
				stack.push_back(Gompl.undefined)
			"bin_logic":
				var l = stack.pop_back()
				if it[pos][2] == "and":
					if not l or l is _Undefined: stack.push_back(false); pos = it[pos][3] - 1
				elif it[pos][2] == "or":
					if l and l is not _Undefined: stack.push_back(true); pos = it[pos][3] - 1
			"bin_logic_end":
				var r = stack.pop_back()
				stack.push_back(r and r is not _Undefined)
			"bin":
				var l = stack.pop_back()
				var r = stack.pop_back()
				if it[pos][2] == "==":
					if _is_string(l) and _is_string(r): stack.push_back(l == r)
					elif _is_number(l) and _is_number(r): stack.push_back(l == r)
					else: stack.push_back(typeof(l) == typeof(r) and l == r)
				elif it[pos][2] == "!=":
					if _is_string(l) and _is_string(r): stack.push_back(l != r)
					elif _is_number(l) and _is_number(r): stack.push_back(l != r)
					else: stack.push_back(typeof(l) != typeof(r) or l != r)
				elif l is _Undefined or r is _Undefined:
					_set_err_runtime(it[pos], "Can't use undefined variable in binary op '" + it[pos][2] + "'")
					stack.push_back(Gompl.undefined)
				elif it[pos][2] in [ "+", "-", "*", "/", "%" ]:
					#print(">>> ", l, " ", it[pos][1], " ", r)
					match it[pos][2]:
						"+":
							if _is_string(l) or _is_string(r): stack.push_back(str(l, r))
							else: stack.push_back(l + r)
						"-":
							if _is_string(l): stack.push_back(l.replace(str(r), ""))
							elif _is_string(r): _set_err_runtime(it[pos], "Incompatible types in binary op '-'"); stack.push_back(Gompl.undefined)
							else: stack.push_back(l - r)
						"*":
							if _is_string(l) and _is_number(r): stack.push_back(l.repeat(r))
							elif _is_number(l) and _is_string(r): stack.push_back(r.repeat(l))
							elif _is_string(l) and _is_string(r): _set_err_runtime(it[pos], "Incompatible types in binary op '*'"); stack.push_back(Gompl.undefined)
							else: stack.push_back(l * r)
						"/":
							if _is_string(l) or _is_string(r): _set_err_runtime(it[pos], "Incompatible types in binary op '/'"); stack.push_back(Gompl.undefined)
							elif r == 0: _set_err_runtime(it[pos], "Division by zero"); stack.push_back(Gompl.undefined)
							else: stack.push_back(l / r)
						"%":
							if _is_string(l) or _is_string(r): _set_err_runtime(it[pos], "Incompatible types in binary op '%'"); stack.push_back(Gompl.undefined)
							elif r == 0: _set_err_runtime(it[pos], "Division by zero"); stack.push_back(Gompl.undefined)
							else: stack.push_back(l % r)
				else:
					if (_is_string(l) and not _is_string(r)) or (not _is_string(l) and _is_string(r)):
						_set_err_runtime(it[pos], "Incompatible types for binary op '" + it[pos][2] + "'"); stack.push_back(Gompl.undefined)
					else:
						match it[pos][2]:
							"<": stack.push_back(l < r)
							"<=": stack.push_back(l <= r)
							">": stack.push_back(l > r)
							">=": stack.push_back(l >= r)
			"unary":
				var r = stack.pop_back()
				match it[pos][2]:
					"not":
						if r is not bool: _set_err_runtime(it[pos], "Incompatible type for unary op 'not'"); stack.push_back(Gompl.undefined)
						else: stack.push_back(not r)
					"-":
						if not _is_number(r): _set_err_runtime(it[pos], "Incompatible type for unary op '-'"); stack.push_back(Gompl.undefined)
						else: stack.push_back(-r)
			"assign":
				var res = stack.pop_back()
				if res == null or res is _Undefined:
					env.erase(it[pos][2])
					stack.push_back(Gompl.undefined)
				else:
					env[it[pos][2]] = res
					stack.push_back(res)
			"literal":
				stack.push_back(it[pos][2])
			"pop":
				stack.pop_back()
			"id":
				stack.push_back(env.get(it[pos][2], Gompl.undefined))
			"check":
				if stack.pop_back(): stack.pop_back()
				else: stack.push_back(Gompl.undefined); pos = it[pos][2] - 1
			"jump":
				pos = it[pos][2] - 1
			"interrupt":
				if state is Dictionary: state["interrupted"] = true
				else: state = &"interrupted"
			"excall":
				var res
				var rf = _registered_funcs.get(it[pos][2])
				if not it[pos][3]:
					if rf: res = rf[0].call()
					else: res = target.call(it[pos][2])
				else:
					var args = []
					var mlm = null if rf else target.get_method_list().filter(func(m: Dictionary) -> bool: return m.name == it[pos][2])[0]
					for i: int in it[pos][3]:
						var arg = stack.pop_back()
						var a = arg if arg is not _Undefined else null
						var incomp := false
						if rf:
							match rf[1][i]:
								T_NUMBER: if a is not int and a is not float: incomp = true
								T_STRING: if a is not String: incomp = true
								T_BOOL: if a is not bool: incomp = true
							if incomp:
								_set_err_runtime(it[pos], str("Incompatible type '", type_string(typeof(a)).to_lower(), "' for parameter ", i + 1, ", wants '", rf[1][i], "'"))
								stack.push_back(Gompl.undefined)
								break
						else:
							match mlm.args[i].type:
								TYPE_INT: if a is not int and a is not float: incomp = true
								TYPE_FLOAT: if a is not int and a is not float: incomp = true
								_: if typeof(a) != mlm.args[i].type and mlm.args[i].type != TYPE_NIL: incomp = true
							if incomp:
								_set_err_runtime(it[pos], str("Incompatible type '", type_string(typeof(a)).to_lower(), "' for parameter ", i + 1, ", wants '", type_string(mlm.args[i].type), "'"))
								stack.push_back(Gompl.undefined)
								break
						args.append(a)
					if not err:
						if rf: res = rf[0].callv(args)
						else: res = target.callv(it[pos][2], args)
				stack.push_back(res if res != null else Gompl.undefined)
		
		pos += 1
		step += 1
		if step >= max_steps:
			if state is Dictionary: state["interrupted"] = true
			else: state = &"interrupted"
	
	if err: printerr(err); return null
	
	if state is Dictionary and state.has("interrupted"):
		state["stack"] = stack
		state["pos"] = pos
		state["env"] = env
		state["steps"] = step
		state.erase("interrupted")
		return null
	elif state is StringName and state == &"interrupted":
		return null
	
	if debug_printing and stack: print("RESULT: ", stack.back())
	if debug_printing: print("ENVIRONMENT: ", env)
	return stack.back() if stack else null

###

func _set_err(e, overwrite := false) -> void:
	if err and not overwrite: return
	err = str(e)
	
func _set_err_runtime(instruction: Array, e: String) -> void:
	var error := str("[Runtime] [Line ", instruction[0], "] ", e)
	_set_err(error, false)

func _is_string(v) -> bool:
	return v is String or v is StringName

func _is_number(v) -> bool:
	return v is int or v is float

### LEXER

func _lex(code: String) -> Array[Array]:
	var pos := 0
	var tokens: Array[Array] = []
	var reg := RegEx.new()
	var line := 1
	while pos < code.length():
		var res: RegExMatch = null
		var tag: String
		for tidx: int in range(0, _TOKEN_EXPRESSIONS.size(), 2):
			reg.compile(_TOKEN_EXPRESSIONS[tidx])
			res = reg.search(code, pos)
			if res and res.get_start() == pos:
				tag = _TOKEN_EXPRESSIONS[tidx + 1]
				var value := res.get_string()
				if tag == _IGNORE:
					line += value.count("\n")
					break
				if tag == _ID:
					if value in _TOKEN_KEYWORDS: tag = _RESERVED
					elif value in _TOKEN_BOOLS: tag = _BOOL
					elif value in _TOKEN_UNDEFINED: tag = _UNDEFINED
				var token: Array = [ value, tag, line ]
				tokens.append(token)
				break
			else:
				res = null
		if res: pos = res.get_end()
		else: _set_err(str("[Lexer] [Line ", line, "] Found illegal character '", code[pos], "' (token ", pos, ")")); return []
	return tokens

### EXPRESSIONS

class _Undefined extends Expr:
	func _init() -> void: pass
	func _to_string() -> String: return "undefined"
	func compile(_gompl: Gompl, it: Array[Array], _scope_stack: Array[Scope]) -> void: it.append([ _line, "undefined" ])

class Scope:
	var start_pos: int
	var stops: Array[Array]
	func _init(p: int) -> void: start_pos = p

class Expr:
	var _line: int
	
	func _set_err(gompl: Gompl, e: String) -> void:
		var error := str("[Compiler] [Line ", _line, "] ", e)
		gompl._set_err(error, false)
		
	func _init(l: int) -> void:
		_line = l
	
	func compile(_gompl: Gompl, _it: Array[Array], _scope_stack: Array[Scope]) -> void:
		pass
	
	# TODO make the operations more robust for different types
	class Binary extends Expr:
		var left: Expr
		var op: String
		var right: Expr
		func _init(ln: int, l: Expr, o: String, r: Expr) -> void: super(ln); left = l; op = o; right = r
		func _to_string() -> String: return str("Binary(", left, ", '", op, "', ", right, ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope]) -> void:
			if left == null: _set_err(gompl, str("Binary op '", op, "' missing left operand")); return
			if right == null: _set_err(gompl, str("Binary op '", op, "' missing right operand")); return
			if op == "and" or op == "or":
				left.compile(gompl, it, scope_stack)
				var d = [ _line, "bin_logic", op ]; it.append(d)
				right.compile(gompl, it, scope_stack)
				it.append([ _line, "bin_logic_end" ])
				d.append(it.size())
			else:
				right.compile(gompl, it, scope_stack)
				left.compile(gompl, it, scope_stack)
				it.append([ _line, "bin", op ])
	class Unary extends Expr:
		var op: String
		var right: Expr
		func _init(ln: int, o: String, r: Expr) -> void: super(ln); op = o; right = r
		func _to_string() -> String: return str("Unary('", op, "', ", right, ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope]) -> void:
			right.compile(gompl, it, scope_stack)
			it.append([ _line, "unary", op ])
	class Assignment extends Expr:
		var left: Identifier
		var op: String
		var right: Expr
		func _init(ln: int, l: Identifier, o: String, r: Expr) -> void: super(ln); left = l; op = o; right = r
		func _to_string() -> String: return str("Assignment(", left, ", '", op, "', ", right, ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope]) -> void:
			right.compile(gompl, it, scope_stack)
			it.append([ _line, "assign", left.name ])
	class Literal extends Expr:
		var lit
		func _init(ln: int, l) -> void: super(ln); lit = l
		func _to_string() -> String: return str("Literal(", lit, ", ", type_string(typeof(lit)), ")")
		func compile(_gompl: Gompl, it: Array[Array], _scope_stack: Array[Scope]) -> void:
			it.append([ _line, "literal", lit ])
	class List extends Expr:
		var exprs: Array[Expr]
		func _init(ln: int, a: Array[Expr]) -> void: super(ln); exprs = a
		func _to_string() -> String: return str("List(", exprs.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope]) -> void:
			for i: int in exprs.size():
				exprs[i].compile(gompl, it, scope_stack)
				if i != exprs.size() - 1: it.append([ _line, "pop" ])
	class Identifier extends Expr:
		var name: String
		func _init(ln: int, n: String) -> void: super(ln); name = n
		func _to_string() -> String: return str("Identifier('", name, "')")
		func compile(_gompl: Gompl, it: Array[Array], _scope_stack: Array[Scope]) -> void:
			it.append([ _line, "id", name ])
	class If extends Expr:
		var conds: Array[Expr]
		var bodies: Array[Expr]
		func _init(ln: int, c: Array[Expr], b: Array[Expr]) -> void: super(ln); conds = c; bodies = b
		func _to_string() -> String: return str("If(", conds, ", ", bodies.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope]) -> void:
			var check: Array
			var jumps: Array[Array]
			for i: int in conds.size():
				if check: check.append(it.size())
				conds[i].compile(gompl, it, scope_stack)
				check = [ _line, "check" ]; it.append(check)
				bodies[i].compile(gompl, it, scope_stack)
				jumps.append([ _line, "jump" ]); it.append(jumps[-1])
			check.append(it.size())
			if bodies.size() > conds.size():
				bodies[-1].compile(gompl, it, scope_stack)
			for j in jumps:
				j.append(it.size())
	class While extends Expr:
		var cond: Expr
		var body: Expr
		func _init(ln: int, c: Expr, b: Expr) -> void: super(ln); cond = c; body = b
		func _to_string() -> String: return str("While(", cond, ", ", body, ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope]) -> void:
			var start_pos := it.size()
			var scope := Scope.new(start_pos)
			scope_stack.push_back(scope)
			cond.compile(gompl, it, scope_stack)
			var check = [ _line, "check" ]; it.append(check)
			body.compile(gompl, it, scope_stack)
			it.append([ _line, "jump", start_pos])
			check.append(it.size())
			for s: Array in scope.stops: s.append(it.size()) # jump targets of stops
			scope_stack.erase(scope)
	class FlowControl extends Expr:
		var op: String
		func _init(ln: int, o: String) -> void: super(ln); op = o
		func _to_string() -> String: return str("Stop()")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope]) -> void:
			if op == "interrupt":
				it.append([ _line, "interrupt" ])
			elif not scope_stack:
				_set_err(gompl, str("Unexpected '", op, "'"))
			else: 
				var jump = [ _line, "jump" ]
				if op == "stop":
					it.append([ _line, "undefined" ])
					scope_stack.back().stops.append(jump)
				elif op == "skip":
					jump.append(scope_stack.back().start_pos)
				it.append(jump)
	class FnCall extends Expr:
		var method: String
		var params: Array[Expr]
		func _init(ln: int, m: String, p: Array[Expr]) -> void: super(ln); method = m; params = p
		func _to_string() -> String: return str("FnCall('", method, "', ", params.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope]) -> void:
			var rf = gompl._registered_funcs.get(method)
			if not rf and (not gompl.target or not gompl.target.has_method(method)):
				_set_err(gompl, str("Can not call function '", method, "'"))
				return
			if rf:
				if params.size() < rf[1].size() - rf[2]: _set_err(gompl, str("Too few parameters for function '", method, "'")); return
				if params.size() > rf[1].size(): _set_err(gompl, str("Too many parameters for function '", method, "'")); return
			else:
				var arg_count := gompl.target.get_method_argument_count(method)
				if params.size() > arg_count:
					_set_err(gompl, str("Too many parameters for function '", method, "'"))
					return
				elif params.size() < arg_count - gompl.target.get_method_list().filter(func(m: Dictionary) -> bool: return m.name == method)[0].default_args.size():
					_set_err(gompl, str("Too few parameters for function '", method, "'"))
					return
			for i: int in range(params.size() -1, -1, -1):
				params[i].compile(gompl, it, scope_stack)
			it.append([ _line, "excall", method, params.size() ])

### PARSER

class Parser:
	var gompl: Gompl
	var tokens: Array[Array]
	var exprs: Array[Expr]
	var pos := 0
	var _err_pos := 0
	
	func _init(g: Gompl, t: Array[Array]) -> void:
		gompl = g
		tokens = t
	
	func _set_err(e: String) -> void:
		var error := str("[Parser] [Line ", tokens[mini(_err_pos, tokens.size() - 1)][2], "] ", e)
		gompl._set_err(error, false)
	
	func expressions() -> Expr:
		var expr: Expr = null
		var array: Array[Expr]
		var ln: int = tokens[pos][2]
		while pos < tokens.size():
			var e := expression()
			if gompl.err: return null
			if not e: break
			array.append(e)
			expr = e
		return Expr.List.new(ln, array) if array.size() > 1 else expr

	func expression() -> Expr:
		return assignment()
	
	func assignment() -> Expr:
		var expr := op_and()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_ASSIGNMENT:
			if expr is not Expr.Identifier: _set_err("Assignment missing left side identifier"); return null
			var ln: int = tokens[pos][2]
			var operator: String = tokens[pos][0]
			pos += 1
			var right := expression()
			if not right: _set_err("Assignment missing right side expression"); return null
			expr = Expr.Assignment.new(ln, expr as Expr.Identifier, operator, right)
		return expr
	
	func op_and() -> Expr:
		var expr := op_or()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_AND:
			var ln: int = tokens[pos][2]
			var operator: String = tokens[pos][0]
			pos += 1
			var right := op_or()
			if not right: _set_err("Binary op 'and' has wrong right side"); return null
			expr = Expr.Binary.new(ln, expr, operator, right)
		return expr
	
	func op_or() -> Expr:
		var expr := equality()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_OR:
			var ln: int = tokens[pos][2]
			var operator: String = tokens[pos][0]
			pos += 1
			var right := equality()
			if not right: _set_err("Binary op 'or' has wrong right side"); return null
			expr = Expr.Binary.new(ln, expr, operator, right)
		return expr
	
	func equality() -> Expr:
		var expr := comparison()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_EQUALITY:
			var ln: int = tokens[pos][2]
			var operator: String = tokens[pos][0]
			pos += 1
			var right := comparison()
			if not right: _set_err("Binary op '" + operator + "' has wrong right side"); return null
			expr = Expr.Binary.new(ln, expr, operator, right)
		return expr
	
	func comparison() -> Expr:
		var expr := term()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_COMPARISON:
			var ln: int = tokens[pos][2]
			var operator: String = tokens[pos][0]
			pos += 1
			var right := term()
			if not right: _set_err("Binary op '" + operator + "' has wrong right side"); return null
			expr = Expr.Binary.new(ln, expr, operator, right)
		return expr
	
	func term() -> Expr:
		var expr := factor()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_TERM:
			var ln: int = tokens[pos][2]
			var operator: String = tokens[pos][0]
			pos += 1
			var right := factor()
			if not right: _set_err("Binary op '" + operator + "' has wrong right side"); return null
			expr = Expr.Binary.new(ln, expr, operator, right)
		return expr
	
	func factor() -> Expr:
		var expr := unary()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_FACTOR:
			var ln: int = tokens[pos][2]
			var operator: String = tokens[pos][0]
			pos += 1
			var right := unary()
			if not right: _set_err("Binary op '" + operator + "' has wrong right side"); return null
			expr = Expr.Binary.new(ln, expr, operator, right)
		return expr
	
	func unary() -> Expr:
		if pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_UNARY:
			var ln: int = tokens[pos][2]
			var operator: String = tokens[pos][0]
			pos += 1
			var right := unary()
			if not right: _set_err("Unary op '" + operator + "' has wrong right side"); return null
			return Expr.Unary.new(ln, operator, right)
		return primary()

	func primary() -> Expr:
		if pos >= tokens.size(): return null
		var res: Expr = null
		_err_pos = pos
		var ln: int = tokens[pos][2]
		match tokens[pos][1]:
			_UNDEFINED: res = Gompl.undefined
			_BOOL: res = Expr.Literal.new(ln, tokens[pos][0] == "true")
			_FLOAT: res = Expr.Literal.new(ln, float(tokens[pos][0]))
			_INT: res = Expr.Literal.new(ln, int(tokens[pos][0]))
			_STRING: res = Expr.Literal.new(ln, tokens[pos][0].substr(1, tokens[pos][0].length() - 2).c_unescape()) # removing the quotation marks
			_ID:
				var ident = tokens[pos][0]
				if pos < tokens.size() - 1 and tokens[pos + 1][0] == "(":
					pos += 2
					var params: Array[Expr] = []
					while pos < tokens.size() and tokens[pos][0] != ")":
						var expr := expression()
						if not expr: _set_err("Expect expression inside params list"); break
						params.append(expr)
						if pos >= tokens.size(): _set_err("Expect ',' or ')' in params list, early EOF"); break
						elif tokens[pos][0] == ",": pos += 1; continue
						elif tokens[pos][0] != ")": _set_err("Expect ',' or ')' in params list"); break
					if not gompl.err:
						if pos >= tokens.size(): _set_err("Expect ',' or ')' in params list, early EOF")
						else: res = Expr.FnCall.new(ln, ident, params)
				else:
					res = Expr.Identifier.new(ln, ident)
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
					if not gompl.err: res = Expr.If.new(ln, conds, bodies)
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
						else: res = Expr.While.new(ln, cond, body)
				elif tokens[pos][0] == "stop":
					res = Expr.FlowControl.new(ln, "stop")
				elif tokens[pos][0] == "skip":
					res = Expr.FlowControl.new(ln, "skip")
				elif tokens[pos][0] == "interrupt":
					res = Expr.FlowControl.new(ln, "interrupt")
				else:
					pass # do nothing, otherwise expressions() always spits out an error
		if res: pos += 1
		return res

	func parse() -> Expr:
		var res := expressions()
		if gompl.err: return null
		return res
