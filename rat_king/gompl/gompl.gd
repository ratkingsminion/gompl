class_name Gompl
extends RefCounted

## A simple interpreted scripting language for Godot
## Based on https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-
## and https://jayconrod.com/posts/65/how-to-build-a-parser-by-hand
## and https://craftinginterpreters.com/parsing-expressions.html

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
	r"\/\/[^\n]*", _IGNORE, # comments
	r",", _RESERVED, # separator
	r"\.", _RESERVED, r"\[", _RESERVED, r"\]", _RESERVED, # access
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
const _TOKEN_ACCESSOR: Array[String] = [ "." ]
const _TOKEN_BINARY_OPERATOR: Array[String] = [ "+", "-", "*", "/", "%" ]
const _TOKEN_BINARY_OPERATOR_ALLOWING_UNDEFINED: Array[String] = [ "+", "%" ]
const _TOKEN_FLOW_CONTROL: Array[String] = [ "stop", "skip", "interrupt" ]
const _TOKEN_KEYWORDS: Array[String] = [ "and", "or", "not", "if", "then", "else", "elif", "while", "do", "end", "stop", "skip", "interrupt", "with", "function", "array", "dictionary" ]
const _TOKEN_UNDEFINED: Array[String] = [ "undefined" ]
const _TOKEN_BOOLS: Array[String] = [ "true", "false" ]

enum Instruction { UNDEFINED, BINARY_LOGIC, BINARY_LOGIC_END, BINARY, UNARY, ASSIGN, ASSIGN_ARR, LITERAL, POP,
	IDENTIFIER, CHECK, JUMP, INTERRUPT, CALL_METHOD, CALL_INTERNAL, RETURN, CALL_EXTERNAL, ARRAY, ACCESS_ARR, DICTIONARY }

var debug_printing := false
var err: String
var target: Object

const T_ANY = &"any"
const T_NUMBER = &"number"
const T_STRING = &"string"
const T_BOOL = &"bool"
const T_ARRAY = &"array"
const T_DICTIONARY = &"dictionary"
const T_UNDEFINED = &"undefined"

var _registered_in_funcs: Dictionary
var _registered_ex_funcs: Dictionary

###

func _init(target_object: Object = null) -> void:
	self.target = target_object

###

## param_types is an array filled with types (T_ANY, T_INT, etc.)
## optional_params is the amount of optional parameters of the function
func register_func(func_name: String, callable: Callable, param_types: Array[StringName] = [], optional_params := 0) -> void:
	if not func_name or not callable: printerr("Invalid function registration"); return
	if func_name in _registered_ex_funcs: printerr("Function name already registered"); return
	_registered_ex_funcs[func_name] = [ callable, param_types, optional_params ]
	
func unregister_func(func_name: String) -> void:
	_registered_ex_funcs.erase(func_name)

## env is a Dictionary that contains all the variables assigned in the code
func eval(code: String, env = null, state = null, max_steps := -1, clear_internal_funcs := true):
	var tokens := tokenize_code(code)
	if not tokens: return null # some error happened
	var ast := parse_tokens(tokens, clear_internal_funcs)
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
func parse_tokens(tokens: Array[Array], clear_internal_funcs := true) -> Expr:
	err = ""
	if clear_internal_funcs: _registered_in_funcs.clear()
	var parser := Parser.new(self, tokens)
	if err: printerr(err); return null
	var ast := parser.parse()
	if err: printerr(err); return null
	if debug_printing: print("AST: ", ast)
	return ast
	
## Step 3 - compile the AST to an arry of instructions
func compile(ast: Expr) -> Array[Array]:
	err = ""
	var it: Array[Array]
	var scope := Scope.new(0)
	ast.compile(self, it, [ scope ])
	if _registered_in_funcs:
		var jump := [ it[-1][0] if it else 0, Instruction.JUMP ]
		scope.stops.append(jump) # final end instruction
		it.append(jump)
		for f: Expr.Function in _registered_in_funcs.values(): f.compile_deferred(self, it)
	scope.init_stops(it.size())
	if err: printerr(err); return []
	if debug_printing and it: print("INSTRUCTIONS: ", it.map(func(a: Array) -> Array: var b: Array = a.duplicate(); b[1] = Instruction.keys()[b[1]]; return b))
	return it

## Step 4 - iterate over the array of instructions, using it as a lower level language
## env is a Dictionary that contains all the variables assigned in the code
## If you don't provide env, a temporary one will be created
## If you provide a state Dictionary, it can be re-used to continue the execution after it was interrupted
func run(it: Array[Array], env = null, state = null, max_steps := -1) -> Variant:
	err = ""
	if env == null: env = {} if state is not Dictionary else state.get(&"env", {})
	elif env is not Dictionary: _set_err("Environment must be a Dictionary"); env = {}
	
	var step: int = 0
	var stack: Array = [] if state is not Dictionary else state.get(&"stack", [])
	var returns: Array = [] if state is not Dictionary else state.get(&"returns", [])
	var pos: int = 0 if state is not Dictionary else state.get(&"pos", 0)
	
	while not err and pos < it.size():
		if state is Dictionary and &"interrupted" in state:
			break
		var line: int = it[pos][0]
		#print("run ", pos, ") ", it[pos], " - stack:", stack, " env:", env)
		match it[pos][1]:
			Instruction.UNDEFINED:
				stack.push_back(Undefined.new(line))
			Instruction.BINARY_LOGIC:
				var l = stack.pop_back()
				if it[pos][2] == "and":
					if not l or l is Undefined: stack.push_back(false); pos = it[pos][3] - 1
				elif it[pos][2] == "or":
					if l and l is not Undefined: stack.push_back(true); pos = it[pos][3] - 1
			Instruction.BINARY_LOGIC_END:
				var r = stack.pop_back()
				stack.push_back(r and r is not Undefined)
			Instruction.BINARY:
				var l = stack.pop_back()
				var r = stack.pop_back()
				if it[pos][2] == "==":
					if _is_string(l) and _is_string(r): stack.push_back(l == r)
					elif _is_number(l) and _is_number(r): stack.push_back(l == r)
					elif l is Undefined and r is Undefined: stack.push_back(true)
					else: stack.push_back(typeof(l) == typeof(r) and l == r)
				elif it[pos][2] == "!=":
					if _is_string(l) and _is_string(r): stack.push_back(l != r)
					elif _is_number(l) and _is_number(r): stack.push_back(l != r)
					elif l is Undefined and r is Undefined: stack.push_back(false)
					else: stack.push_back(typeof(l) != typeof(r) or l != r)
				elif (l is Undefined or r is Undefined) and it[pos][2] not in _TOKEN_BINARY_OPERATOR_ALLOWING_UNDEFINED:
					_set_err_runtime(it[pos], "Can't use undefined variable in binary op '" + it[pos][2] + "'")
					stack.push_back(Undefined.new(line))
				elif it[pos][2] in _TOKEN_BINARY_OPERATOR:
					match it[pos][2]:
						"+":
							if _is_string(l) or _is_string(r): stack.push_back(str(l, r))
							elif l is Undefined or r is Undefined: stack.push_back(Undefined.new(line))
							else: stack.push_back(l + r)
						"-":
							if _is_string(l): stack.push_back(l.replace(str(r), ""))
							elif _is_string(r): _set_err_runtime(it[pos], "Incompatible types in binary op '-'"); stack.push_back(Undefined.new(line))
							elif l is Undefined or r is Undefined: stack.push_back(Undefined.new(line))
							else: stack.push_back(l - r)
						"*":
							if _is_string(l) and _is_number(r): stack.push_back(l.repeat(r))
							elif _is_number(l) and _is_string(r): stack.push_back(r.repeat(l))
							elif _is_string(l) and _is_string(r): _set_err_runtime(it[pos], "Incompatible types in binary op '*'"); stack.push_back(Undefined.new(line))
							else: stack.push_back(l * r)
						"/":
							if _is_string(l) or _is_string(r): _set_err_runtime(it[pos], "Incompatible types in binary op '/'"); stack.push_back(Undefined.new(line))
							elif l is Undefined or r is Undefined: stack.push_back(Undefined.new(line))
							elif _is_number(r) and r == 0: _set_err_runtime(it[pos], "Division by zero"); stack.push_back(Undefined.new(line))
							else: stack.push_back(l / r)
						"%":
							if _is_string(r) and not _is_string(l): _set_err_runtime(it[pos], "Incompatible types in binary op '%'"); stack.push_back(Undefined.new(line))
							elif l is Undefined or r is Undefined: stack.push_back(Undefined.new(line))
							elif _is_number(r) and r == 0: _set_err_runtime(it[pos], "Division by zero"); stack.push_back(Undefined.new(line))
							else: stack.push_back(l % r)
				else:
					if _is_string(l) != _is_string(r):
						_set_err_runtime(it[pos], "Incompatible types for binary op '" + it[pos][2] + "'"); stack.push_back(Undefined.new(line))
					else:
						match it[pos][2]:
							"<": stack.push_back(l < r)
							"<=": stack.push_back(l <= r)
							">": stack.push_back(l > r)
							">=": stack.push_back(l >= r)
			Instruction.UNARY:
				var r = stack.pop_back()
				match it[pos][2]:
					"not":
						stack.push_back(r is Undefined or not r)
					"-":
						if r is Undefined: stack.push_back(Undefined.new(line))
						elif not _is_number(r): _set_err_runtime(it[pos], "Incompatible type for unary op '-'"); stack.push_back(Undefined.new(line))
						else: stack.push_back(-r)
			Instruction.ASSIGN:
				var res = stack.pop_back()
				if res == null or res is Undefined:
					env.erase(it[pos][2])
					stack.push_back(Undefined.new(line))
				else:
					env[it[pos][2]] = res
					stack.push_back(res)
			Instruction.ASSIGN_ARR:
				var arr = stack.pop_back()
				var idx = stack.pop_back()
				var res = stack.back()
				if res is Undefined: res = null
				if arr is Array and (idx >= arr.size() or idx < -arr.size()): _set_err_runtime(it[pos], str("Array access out of bounds")); stack.push_back(Undefined.new(line)) 
				else: arr[idx] = res
			Instruction.LITERAL:
				stack.push_back(it[pos][2])
			Instruction.POP:
				stack.pop_back()
			Instruction.IDENTIFIER:
				stack.push_back(env.get(it[pos][2], Undefined.new(line)))
			Instruction.CHECK: # conditional jump
				if stack:
					var r = stack.pop_back()
					if r and r is not Undefined: stack.pop_back()
					else: pos = it[pos][2] - 1
				else:
					stack.push_back(Undefined.new(line)); pos = it[pos][2] - 1
			Instruction.JUMP:
				pos = it[pos][2] - 1
			Instruction.INTERRUPT:
				var res = stack.pop_back() if it[pos][2] else null
				if state is Dictionary: state[&"interrupted"] = res
				else: state = { &"interrupted": res }
			Instruction.CALL_METHOD:
				var obj = stack.pop_back() # get the object to access with a method
				var f: String = it[pos][2]
				match typeof(obj):
					#TYPE_OBJECT: _excall(stack, line, it[pos], obj) # unsafe!
					TYPE_ARRAY: _excall(stack, line, it[pos], ArrayProxy.new(obj))
					TYPE_DICTIONARY: _excall(stack, line, it[pos], DictionaryProxy.new(obj))
					_: _set_err_runtime(it[pos], str("Invalid target object for method '", f, "'")); stack.push_back(Undefined.new(line)) 
			Instruction.CALL_INTERNAL:
				var rf: Expr.Function = _registered_in_funcs.get(it[pos][2])
				returns.push_back(pos)
				pos = rf.start_pos - 1
			Instruction.RETURN:
				pos = returns.pop_back()
			Instruction.CALL_EXTERNAL:
				var rf = _registered_ex_funcs.get(it[pos][2])
				_excall(stack, line, it[pos], target, rf)
			Instruction.ARRAY:
				var res: Array; res.resize(it[pos][2])
				for i: int in it[pos][2]:
					var elem = stack.pop_back()
					res[i] = elem if elem is not Undefined else null
				stack.push_back(res)
			Instruction.ACCESS_ARR:
				var idx = stack.pop_back()
				var arr = stack.pop_back()
				if _is_container(arr) or _is_string(arr):
					if _is_str_or_arr(arr) and not _is_number(idx): _set_err_runtime(it[pos], str("Array access must be a number")); stack.push_back(Undefined.new(line)) 
					elif _is_str_or_arr(arr) and (idx >= len(arr) or idx < -len(arr)): _set_err_runtime(it[pos], str("Array access out of bounds")); stack.push_back(Undefined.new(line)) 
					elif not it[pos][2] and arr is Dictionary and idx not in arr: _set_err_runtime(it[pos], str("Dictionary access needs existing key")); stack.push_back(Undefined.new(line)) 
					elif it[pos][2] and _is_container(arr): stack.push_back(idx); stack.push_back(arr) # is left side
					elif it[pos][2]: _set_err_runtime(it[pos], str("Can't use String array access on left side")); stack.push_back(Undefined.new(line)) 
					else: stack.push_back(arr[idx])
				else: _set_err_runtime(it[pos], str("Invalid array access")); stack.push_back(Undefined.new(line))
			Instruction.DICTIONARY:
				var res: Dictionary
				for i: int in range(0, it[pos][2], 2):
					var key = stack.pop_back() # TODO
					var val = stack.pop_back()
					res[key if key is not Undefined else null] = val if val is not Undefined else null
				stack.push_back(res)
		
		pos += 1
		step += 1
		if max_steps > 0 and step >= max_steps:
			if state is Dictionary: state[&"interrupted"] = null
			else: state = { &"interrupted": null }
	
	if err: printerr(err); return null
	
	if state is Dictionary and &"interrupted" in state:
		state[&"stack"] = stack
		state[&"returns"] = returns
		state[&"pos"] = pos
		state[&"env"] = env
		state[&"steps"] = step
		var res = state[&"interrupted"]
		state.erase(&"interrupted")
		return res
	
	if debug_printing and stack: print("RESULT: ", stack.back())
	if debug_printing: print("ENVIRONMENT: ", env)
	return stack.back() if stack else null

func _excall(stack: Array, line: int, it_at_pos: Array, t, rf = null):
	var res
	var mn: String = it_at_pos[2]
	if t is Proxy: mn = str(t._prefix, mn) # proxy call for safe object access
	if not rf and not (t and t.has_method(mn)): # check existence of method again - TODO always?
		_set_err_runtime(it_at_pos, str("Method '", it_at_pos[2], "' not found"))
		stack.push_back(Undefined.new(line))
		return null
	var args = []
	var mlm = null if rf else t.get_method_list().filter(func(m: Dictionary) -> bool: return m.name == mn)[0]
	if not rf: # check argument count again - TODO always?
		if it_at_pos[3] > mlm.args.size():
			_set_err_runtime(it_at_pos, str("Too many parameters for method '", mn, "'"))
			stack.push_back(Undefined.new(line))
		elif it_at_pos[3] < mlm.args.size() - mlm.default_args.size():
			_set_err_runtime(it_at_pos, str("Too few parameters for method '", mn, "'"))
			stack.push_back(Undefined.new(line))
	if not err:
		for i: int in it_at_pos[3]:
			var arg = stack.pop_back()
			var a = arg if arg is not Undefined else null
			var incomp := false
			if rf:
				match rf[1][i]:
					T_NUMBER: if not _is_number(a): incomp = true
					T_STRING: if not _is_string(a): incomp = true
					T_BOOL: if a is not bool: incomp = true
					T_ARRAY: if a is not Array: incomp = true
					T_DICTIONARY: if a is not Dictionary: incomp = true
				if incomp:
					_set_err_runtime(it_at_pos, str("Incompatible type '", type_string(typeof(a)).to_lower(), "' for parameter ", i + 1, ", wants '", rf[1][i], "'"))
					stack.push_back(Undefined.new(line))
					break
			else:
				match mlm.args[i].type:
					TYPE_INT, TYPE_FLOAT: if not _is_number(a): incomp = true
					TYPE_STRING, TYPE_STRING_NAME: if not _is_string(a): incomp = true
					TYPE_ARRAY: if a is not Array: incomp = true
					_: if typeof(a) != mlm.args[i].type and mlm.args[i].type != TYPE_NIL: incomp = true
				if incomp:
					_set_err_runtime(it_at_pos, str("Incompatible type '", type_string(typeof(a)).to_lower(), "' for parameter ", i + 1, ", wants '", type_string(mlm.args[i].type), "'"))
					stack.push_back(Undefined.new(line))
					break
			args.append(a)
		if not err:
			if rf: res = rf[0].callv(args)
			else: res = t.callv(mn, args)
	stack.push_back(res if res != null else Undefined.new(line))
	return res

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

func _is_str_or_arr(v) -> bool:
	return v is Array or v is String or v is StringName

func _is_container(v) -> bool:
	return v is Array or v is Dictionary

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

class Undefined extends Expr:
	func _to_string() -> String: return "undefined"
	func compile(_gompl: Gompl, it: Array[Array], _scope_stack: Array[Scope], _parent: Expr = null) -> void: it.append([ _line, Instruction.UNDEFINED ])

class Scope:
	var start_pos: int
	var stops: Array[Array]
	func _init(p: int) -> void: start_pos = p
	func init_stops(p: int) -> void: for s: Array in stops: s.append(p) # jump targets of stops

class Proxy:
	var _prefix: String
	func _init(prefix := "_proxy_") -> void:
		_prefix = prefix

class ArrayProxy extends Proxy:
	var array: Array
	# no filter, map, reduce
	func _init(a: Array) -> void: super(); array = a
	func _proxy_append(e) -> Array: array.append(e); return array
	func _proxy_append_array(a: Array) -> Array: array.append_array(a); return array
	func _proxy_assign(a: Array) -> Array: array.assign(a); return array
	func _proxy_back(): return array[-1] if array else null
	func _proxy_bsearh(val, before := true) -> int: return array.bsearch(val, before)
	func _proxy_clear() -> Array: array.clear(); return array
	func _proxy_count(val) -> int: return array.count(val)
	func _proxy_duplicate(deep := false) -> Array: return array.duplicate(deep)
	func _proxy_erase(val) -> Array: array.erase(val); return array
	func _proxy_fill(val) -> Array: array.fill(val); return array
	func _proxy_find(val, from := 0) -> int: return array.find(val, from)
	func _proxy_front(): return array[0] if array else null
	func _proxy_get(idx: int): return array[idx] if idx < array.size() and idx >= -array.size() else null
	func _proxy_has(val) -> bool: return val in array
	func _proxy_hash() -> int: return array.hash()
	func _proxy_insert(idx: int, val) -> Array: array.insert(idx, val); return array # TODO error check?
	func _proxy_is_empty() -> bool: return array.is_empty()
	func _proxy_max(): return array.max()
	func _proxy_min(): return array.min()
	func _proxy_pick_random(): return array.pick_random()
	func _proxy_pop_at(idx: int): return array.pop_at(idx) if idx < array.size() and idx >= -array.size() else null
	func _proxy_pop_back(): return array.pop_back()
	func _proxy_pop_front(): return array.pop_front()
	func _proxy_push_back(val) -> Array: array.push_back(val); return array
	func _proxy_push_front(val) -> Array: array.push_front(val); return array
	func _proxy_remove_at(idx: int) -> Array:
		if idx < array.size() and idx >= -array.size(): array.remove_at(idx);
		return array
	func _proxy_resize(sz: int) -> Array: array.resize(sz); return array
	func _proxy_reverse() -> Array: array.reverse(); return array
	func _proxy_rfind(val, from := 0) -> int: return array.rfind(val, from)
	func _proxy_set(idx: int, val) -> Array:
		if idx < array.size() and idx >= -array.size(): array[idx] = val
		return array
	func _proxy_shuffle() -> Array: array.shuffle(); return array
	func _proxy_size() -> int: return array.size()
	func _proxy_slice(begin: int, end := 0x7FFFFFFF, step := 1, deep := false) -> Array: return array.slice(begin, end, step, deep)
	func _proxy_sort() -> Array: array.sort(); return array

class DictionaryProxy extends Proxy:
	var dict: Dictionary
	func _init(d: Dictionary) -> void: super(); dict = d
	func _proxy_assign(d: Dictionary) -> Dictionary: dict.assign(d); return dict
	func _proxy_clear() -> Dictionary: dict.clear(); return dict
	func _proxy_duplicate(deep := false) -> Dictionary: return dict.duplicate(deep)
	func _proxy_erase(key) -> Dictionary: dict.erase(key); return dict
	func _proxy_find_key(val): return dict.find_key(val)
	func _proxy_get(key, default = null): return dict.get(key, default)
	func _proxy_get_or_add(key, default = null): return dict.get_or_add(key, default)
	func _proxy_has(key) -> bool: return dict.has(key)
	func _proxy_has_all(keys: Array) -> bool: return dict.has_all(keys)
	func _proxy_hash() -> int: return dict.hash()
	func _proxy_is_empty() -> bool: return dict.is_empty()
	func _proxy_keys() -> Array: return dict.keys()
	func _proxy_merge(d: Dictionary, overwrite := false) -> Dictionary: dict.merge(d, overwrite); return dict
	func _proxy_merged(d: Dictionary, overwrite := false) -> Dictionary: return dict.merged(d, overwrite)
	func _proxy_recursive_equal(d: Dictionary, recursion_count := 100) -> bool: return dict.recursive_equal(d, recursion_count)
	func _proxy_set(key, val) -> Dictionary: dict.set(key, val); return dict # change return value of set()
	func _proxy_size() -> int: return dict.size()
	func _proxy_sort() -> Dictionary: dict.sort(); return dict
	func _proxy_values() -> Array: return dict.values()

class Expr:
	var _line: int
	
	func _set_err(gompl: Gompl, e: String) -> void:
		var error := str("[Compiler] [Line ", _line, "] ", e)
		gompl._set_err(error, false)
		
	func _init(l: int) -> void:
		_line = l
	
	func compile(_gompl: Gompl, _it: Array[Array], _scope_stack: Array[Scope], _parent: Expr = null) -> void:
		pass
	
	# TODO make the operations more robust for different types
	class Binary extends Expr:
		var left: Expr
		var op: String
		var right: Expr
		func _init(ln: int, l: Expr, o: String, r: Expr) -> void: super(ln); left = l; op = o; right = r
		func _to_string() -> String: return str("Binary(", left, ", '", op, "', ", right, ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			if left == null: _set_err(gompl, str("Binary op '", op, "' missing left operand")); return
			if right == null: _set_err(gompl, str("Binary op '", op, "' missing right operand")); return
			if op == "and" or op == "or":
				left.compile(gompl, it, scope_stack)
				var d = [ _line, Instruction.BINARY_LOGIC, op ]; it.append(d)
				right.compile(gompl, it, scope_stack)
				it.append([ _line, Instruction.BINARY_LOGIC_END ])
				d.append(it.size())
			else:
				right.compile(gompl, it, scope_stack)
				left.compile(gompl, it, scope_stack)
				it.append([ _line, Instruction.BINARY, op ])
	class Unary extends Expr:
		var op: String
		var right: Expr
		func _init(ln: int, o: String, r: Expr) -> void: super(ln); op = o; right = r
		func _to_string() -> String: return str("Unary('", op, "', ", right, ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			right.compile(gompl, it, scope_stack)
			it.append([ _line, Instruction.UNARY, op ])
	class Accessor extends Expr:
		var left: Expr
		var right: Expr
		func _init(ln: int, l: Expr, r: Expr) -> void: super(ln); left = l; right = r
		func _to_string() -> String: return str("Accessor(", left, ", ", right, ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			if left == null: _set_err(gompl, str("Accessor missing left operand")); return
			if right == null: _set_err(gompl, str("Accessor missing right operand")); return
			right.compile(gompl, it, scope_stack, left)
	class Assignment extends Expr:
		var left: Expr
		var op: String
		var right: Expr
		func _init(ln: int, l: Expr, o: String, r: Expr) -> void: super(ln); left = l; op = o; right = r
		func _to_string() -> String: return str("Assignment(", left, ", '", op, "', ", right, ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			right.compile(gompl, it, scope_stack)
			if left is Expr.Identifier: it.append([ _line, Instruction.ASSIGN, left.name ])
			elif left is Expr.ArrayAccess: left.compile(gompl, it, scope_stack); it.append([ _line, Instruction.ASSIGN_ARR ])
	class Literal extends Expr:
		var lit
		func _init(ln: int, l) -> void: super(ln); lit = l
		func _to_string() -> String: return str("Literal(", lit, ", ", type_string(typeof(lit)), ")")
		func compile(_gompl: Gompl, it: Array[Array], _scope_stack: Array[Scope], _parent: Expr = null) -> void:
			it.append([ _line, Instruction.LITERAL, lit ])
	class List extends Expr:
		var exprs: Array[Expr]
		func _init(ln: int, a: Array[Expr]) -> void: super(ln); exprs = a
		func _to_string() -> String: return str("List(", exprs.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			var p := 0
			for i: int in exprs.size():
				if exprs[i] is not Function:
					if p != 0: it.append([ _line, Instruction.POP ])
					p += 1
				exprs[i].compile(gompl, it, scope_stack)
	class Identifier extends Expr:
		var name: String
		func _init(ln: int, n: String) -> void: super(ln); name = n
		func _to_string() -> String: return str("Identifier('", name, "')")
		func compile(_gompl: Gompl, it: Array[Array], _scope_stack: Array[Scope], _parent: Expr = null) -> void:
			it.append([ _line, Instruction.IDENTIFIER, name ])
	class If extends Expr:
		var conds: Array[Expr]
		var bodies: Array[Expr]
		func _init(ln: int, c: Array[Expr], b: Array[Expr]) -> void: super(ln); conds = c; bodies = b
		func _to_string() -> String: return str("If(", conds, ", ", bodies.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			var jumps: Array[Array]
			for i: int in conds.size():
				conds[i].compile(gompl, it, scope_stack)
				var check := [ _line, Instruction.CHECK ]; it.append(check)
				bodies[i].compile(gompl, it, scope_stack)
				jumps.append([ _line, Instruction.JUMP ]); it.append(jumps[-1])
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
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			var start_pos := it.size()
			var scope := Scope.new(start_pos)
			scope_stack.push_back(scope)
			cond.compile(gompl, it, scope_stack)
			var check := [ _line, Instruction.CHECK ]; it.append(check)
			body.compile(gompl, it, scope_stack)
			it.append([ _line, Instruction.JUMP, start_pos ])
			check.append(it.size())
			scope.init_stops(it.size())
			scope_stack.erase(scope)
	class FlowControl extends Expr:
		var with: Expr
		var op: String
		func _init(ln: int, o: String, w: Expr) -> void: super(ln); op = o; with = w
		func _to_string() -> String: return str("Stop(", str(with) if with else "", ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			if op == "interrupt":
				if with: with.compile(gompl, it, [])
				it.append([ _line, Instruction.INTERRUPT, true if with else false ])
			elif not scope_stack:
				_set_err(gompl, "Unexpected '" + op + "'")
			else: 
				var jump := [ _line, Instruction.JUMP ]
				if op == "stop":
					if with: with.compile(gompl, it, [])
					scope_stack.back().stops.append(jump)
				elif op == "skip":
					if with: with.compile(gompl, it, [])
					jump.append(scope_stack.back().start_pos)
				it.append(jump)
	class Function extends Expr:
		var body: Expr
		var start_pos: int
		func _init(ln: int, b: Expr) -> void: super(ln); body = b
		func _to_string() -> String: return str("Function(", body, ")")
		func compile_deferred(gompl: Gompl, it: Array[Array]) -> void:
			start_pos = it.size()
			var scope := Scope.new(start_pos)
			body.compile(gompl, it, [ scope ])
			scope.init_stops(it.size())
			it.append([ _line, Instruction.RETURN ])
	class FnCall extends Expr:
		var method: String
		var params: Array[Expr]
		func _init(ln: int, m: String, p: Array[Expr]) -> void: super(ln); method = m; params = p
		func _to_string() -> String: return str("FnCall('", method, "', ", params.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], parent: Expr = null) -> void:
			if parent:
				# method calls can't validate argument count during compilation
				for i: int in range(params.size() -1, -1, -1):
					params[i].compile(gompl, it, scope_stack)
				parent.compile(gompl, it, scope_stack)
				it.append([ _line, Instruction.CALL_METHOD, method, params.size() ])
			else:
				var rf = gompl._registered_in_funcs.get(method)
				if rf: # internal call
					if params.size() > 0: _set_err(gompl, str("Too many parameters for function '", method, "'")); return
					it.append([ _line, Instruction.CALL_INTERNAL, method, params.size() ])
				else:
					rf = gompl._registered_ex_funcs.get(method)
					var res
					if rf: # external call
						if params.size() > rf[1].size(): _set_err(gompl, str("Too many parameters for function '", method, "'"))
						elif params.size() < rf[1].size() - rf[2]: _set_err(gompl, str("Too few parameters for function '", method, "'"))
						else: res = [ _line, Instruction.CALL_EXTERNAL, method, params.size() ]
					elif gompl.target and gompl.target.has_method(method): # target method call
						var f: Dictionary = gompl.target.get_method_list().filter(func(m: Dictionary) -> bool: return m.name == method)[0]
						if params.size() > f.args.size(): _set_err(gompl, str("Too many parameters for function '", method, "'"))
						elif params.size() < f.args.size() - f.default_args.size(): _set_err(gompl, str("Too few parameters for function '", method, "'"))
						else: res = [ _line, Instruction.CALL_EXTERNAL, method, params.size() ]
					else:
						_set_err(gompl, str("Invalid usage of function call for '", method, "'"))
					if res:
						for i: int in range(params.size() -1, -1, -1): params[i].compile(gompl, it, scope_stack)
						it.append(res)
	class NewArray extends Expr:
		var params: Array[Expr]
		func _init(ln: int, p: Array[Expr]) -> void: super(ln); params = p
		func _to_string() -> String: return str("NewArray(", params.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			for i: int in range(params.size() -1, -1, -1):
				params[i].compile(gompl, it, scope_stack)
			it.append([ _line, Instruction.ARRAY, params.size() ])
	class ArrayAccess extends Expr:
		var arr: Expr
		var idx: Expr
		var is_left_side := false
		func _init(ln: int, a: Expr, i: Expr) -> void: super(ln); arr = a; idx = i
		func _to_string() -> String: return str("ArrayAccess('", idx, ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			arr.compile(gompl, it, scope_stack)
			idx.compile(gompl, it, scope_stack)
			it.append([ _line, Instruction.ACCESS_ARR, is_left_side ])
	class NewDictionary extends Expr:
		var params: Array[Expr]
		func _init(ln: int, p: Array[Expr]) -> void: super(ln); params = p
		func _to_string() -> String: return str("NewDictionary(", params.map(func(i): return i), ")")
		func compile(gompl: Gompl, it: Array[Array], scope_stack: Array[Scope], _parent: Expr = null) -> void:
			for i: int in range(params.size() -1, -1, -1):
				params[i].compile(gompl, it, scope_stack)
			it.append([ _line, Instruction.DICTIONARY, params.size() ])

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
	
	func expressions(expected_reserved = null) -> Expr:
		var expr: Expr = null
		var array: Array[Expr]
		var ln: int = tokens[pos][2]
		while pos < tokens.size():
			if expected_reserved and tokens[pos][0] in expected_reserved: break
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
			if expr is not Expr.Identifier and expr is not Expr.ArrayAccess:
				_set_err("Assignment missing left side identifier"); return null
			elif expr is Expr.ArrayAccess:
				expr.is_left_side = true
			var ln: int = tokens[pos][2]
			var operator: String = tokens[pos][0]
			pos += 1
			var right := expression()
			if not right: _set_err("Assignment missing right side expression"); return null
			expr = Expr.Assignment.new(ln, expr, operator, right)
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
		return accessor()
	
	func accessor() -> Expr:
		var expr := array_access()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] in _TOKEN_ACCESSOR:
			var ln: int = tokens[pos][2]
			pos += 1
			var right := primary()
			if not right: _set_err("Accessor has wrong right side"); return null
			expr = Expr.Accessor.new(ln, expr, right)
		return expr
	
	func array_access() -> Expr:
		var expr := primary()
		while pos < tokens.size() and tokens[pos][1] == _RESERVED and tokens[pos][0] == "[":
			var ln: int = tokens[pos][2]
			pos += 1
			var idx := expression()
			if not idx: _set_err("Expect expression inside array access")
			elif pos >= tokens.size(): _set_err("Expect ']' after expression, early EOF")
			elif tokens[pos][0] != "]": _set_err("Expect ']' after expression")
			else: pos += 1; expr = Expr.ArrayAccess.new(ln, expr, idx)
		return expr

	func primary() -> Expr:
		var tcount := tokens.size()
		if pos >= tcount: return null
		var res: Expr = null
		_err_pos = pos
		var ln: int = tokens[pos][2]
		match tokens[pos][1]:
			_UNDEFINED: res = Undefined.new(ln)
			_BOOL: res = Expr.Literal.new(ln, tokens[pos][0] == "true")
			_FLOAT: res = Expr.Literal.new(ln, float(tokens[pos][0]))
			_INT: res = Expr.Literal.new(ln, int(tokens[pos][0]))
			_STRING: res = Expr.Literal.new(ln, tokens[pos][0].substr(1, tokens[pos][0].length() - 2).c_unescape()) # removing the quotation marks
			_ID:
				var ident = tokens[pos][0]
				var params = _group()
				if params != null: res = Expr.FnCall.new(ln, ident, params)
				elif not gompl.err: res = Expr.Identifier.new(ln, ident)
			_RESERVED:
				if tokens[pos][0] == "(":
					pos += 1
					var expr := expression()
					if not expr: _set_err("Expect expression inside group")
					elif pos >= tcount: _set_err("Expect ')' after expression, early EOF")
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
						elif pos >= tcount: _set_err("Expect 'then' after '" + expected + "' condition, early EOF"); break
						elif tokens[pos][0] != "then": _set_err("Expect 'then' after '" + expected + "' condition"); break
						conds.append(cond)
						pos += 1
						var body := expressions([ "elif", "else", "end" ])
						if not body: _set_err("Expect body after 'then'"); break
						elif pos >= tcount: _set_err("Expect 'elif', 'else' or 'end' after " + expected + "-body, early EOF"); break
						bodies.append(body)
						expected = "elif"
					if pos < tcount and tokens[pos][0] == "else":
						pos += 1
						var body_else := expressions([ "end" ])
						if not body_else: _set_err("Expect body after 'else'")
						elif pos >= tcount : _set_err("Expect 'end' after else-body, early EOF")
						else: bodies.append(body_else)
					if not gompl.err:
						res = Expr.If.new(ln, conds, bodies)
				elif tokens[pos][0] == "while":
					pos += 1
					var cond := expression()
					if not cond: _set_err("Expect condition after 'while'")
					elif pos >= tcount: _set_err("Expect 'do' after 'while' condition, early EOF")
					elif tokens[pos][0] != "do": _set_err("Expect 'do' after 'while' condition")
					else:
						pos += 1
						var body := expressions([ "end" ])
						if not body: _set_err("Expect body after 'do'")
						elif pos >= tcount: _set_err("Expect 'end' after while-body, early EOF")
						else: res = Expr.While.new(ln, cond, body)
				elif tokens[pos][0] in _TOKEN_FLOW_CONTROL:
					var fc: String = tokens[pos][0]
					var with: Expr
					if pos < tcount - 1 and tokens[pos + 1][0] == "with":
						pos += 2
						with = expression()
						if not with: _set_err("Expect expression after 'with'")
						else: pos -= 1
					if not gompl.err:
						res = Expr.FlowControl.new(ln, fc, with)
				elif tokens[pos][0] == "function":
					pos += 1
					if pos >= tcount: _set_err("Expect identifier after 'function', early EOF")
					elif tokens[pos][1] != _ID: _set_err("Expect identifier after 'function'")
					else:
						var ident: String = tokens[pos][0]
						if pos >= tcount - 1: _set_err("Expect '(' after identifier, early EOF")
						elif tokens[pos + 1][0] != "(": _set_err("Expect '(' after identifier")
						else:
							pos += 1
							if pos >= tcount - 1: _set_err("Expect ')' after '(', early EOF")
							elif tokens[pos + 1][0] != ")": _set_err("Expect ')' after '('")
							else:
								pos += 2
								var body := expressions([ "end" ])
								if not body: _set_err("Expect body after ')'")
								elif pos >= tcount: _set_err("Expect 'end' after function-body, early EOF")
								else:
									res = Expr.Function.new(ln, body)
									gompl._registered_in_funcs[ident] = res
				elif tokens[pos][0] == "array":
					var params = _group()
					if params != null: res = Expr.NewArray.new(ln, params)
					else: _set_err("Expect '(' after 'array'.")
				elif tokens[pos][0] == "dictionary":
					var params = _group()
					if params != null: res = Expr.NewDictionary.new(ln, params)
					else: _set_err("Expect '(' after 'dictionary'.")
				else:
					_set_err("Unexpected keyword '" + tokens[pos][0] + "'")
					pos += 1
		if res: pos += 1
		return res
	
	func _group(): # returns Array[Expr] or null 
		var tcount := tokens.size()
		if pos < tcount - 1 and tokens[pos + 1][0] == "(":
			pos += 2
			var params: Array[Expr] = []
			while pos < tcount and tokens[pos][0] != ")":
				var expr := expression()
				if not expr: _set_err("Expect expression inside params list"); break
				params.append(expr)
				if pos >= tcount: _set_err("Expect ',' or ')' in params list, early EOF"); break
				elif tokens[pos][0] == ",": pos += 1; continue
				elif tokens[pos][0] != ")": _set_err("Expect ',' or ')' in params list"); break
			if not gompl.err:
				if pos >= tcount: _set_err("Expect ',' or ')' in params list, early EOF")
				else: return params
		return null

	func parse() -> Expr:
		var res := expressions()
		if gompl.err: return null
		return res
