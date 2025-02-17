class_name Gimpl
extends RefCounted

## A simple interpreted scripting language for Godot
## Based on https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-

const _RESERVED := "RESERVED"
const _INT := "INT"
const _STRING := "STRING"
const _BOOL := "BOOL"
const _ID := "ID"
const _TOKEN_EXPRESSIONS: Array[String] = [
	r"[ \n\t]+", "", r"#[^\n]*", "", # whitespaces
	r";", _RESERVED, # separator
	r"\+", _RESERVED, r"-", _RESERVED, r"\*", _RESERVED, r"/", _RESERVED,
	r"<=", _RESERVED, r"<", _RESERVED, r">=", _RESERVED, r">", _RESERVED,
	r"==", _RESERVED, r"!=", _RESERVED,
	r"\=", _RESERVED, r"\(", _RESERVED, r"\)", _RESERVED,
	r"and", _RESERVED, r"or", _RESERVED, r"not", _RESERVED,
	r"if", _RESERVED, r"then", _RESERVED, r"else", _RESERVED,
	r"while", _RESERVED, r"do", _RESERVED, r"end", _RESERVED,
	r"[0-9]+", _INT,
	r"\"(.*?(?<!\\))\"", _STRING,
	r"true", _BOOL, r"false", _BOOL,
	r"[A-Za-z_][A-Za-z0-9_]*", _ID
]

const _aexp_precedence_levels: Array[Array] = [ ['*', '/'], ['+', '-'] ]
const _bexp_precedence_levels: Array[Array] = [ ['and'], ['or'] ]
const _relops: Array[String] = ['<', '<=', '>', '>=', '==', '!=']

var phrase: Parser

###

func _init() -> void:
	phrase = Phrase.new(_exp_list())

###

func eval(code: String) -> bool:
	var tokens := _lex(code, _TOKEN_EXPRESSIONS)
	if not tokens: return false
	var parse_res := _imp_parse(tokens)
	if not parse_res or not parse_res.value: return false
	var env := {}
	var eval_res = parse_res.value.eval(env)
	print(env)
	print(eval_res)
	return true

### TOP-LEVEL

func _imp_parse(tokens: Array[Array]) -> ParseRes:
	return phrase.fn(tokens, 0)

### LEXER

func _lex(code: String, token_exprs: Array[String]) -> Array[Array]:
	var pos := 0
	var tokens: Array[Array] = []
	var reg := RegEx.new()
	while pos < code.length():
		var res: RegExMatch = null
		for tidx: int in range(0, token_exprs.size(), 2):
			reg.compile(token_exprs[tidx])
			res = reg.search(code, pos)
			if res and res.get_start() == pos:
				var tag := token_exprs[tidx + 1]
				if tag: # i.e. is not whitespace to ignore
					var token: Array[String] = [ res.get_string(), tag ]
					tokens.append(token)
				break
		if not res:
			printerr("Illegal character: ", code[pos], " at ", pos)
			return []
		else:
			pos = res.get_end()
	return tokens

### PARSER COMBINATORS

class ParseRes:
	var value
	var pos: int
	func _init(v, p: int) -> void: value = v; pos = p
	func _to_string() -> String: return str("Result(", value, ", ", pos, ")")

class Parser:
	func fn(_tokens: Array[Array], _pos: int) -> ParseRes:
		return null
	func concat(other: Parser) -> Parser: # +
		return Concat.new(self, other)
	func splice(other: Parser) -> Parser: # *
		return Splice.new(self, other)
	func alternate(other: Parser) -> Parser: # |
		return Alternate.new(self, other)
	func process(function: Callable) -> Parser: # ^
		return Process.new(self, function)

class Reserved extends Parser:
	var value: String
	var tag: String
	func _init(v: String, t: String) -> void: value = v; tag = t
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		if pos < tokens.size() and tokens[pos][0] == value and tokens[pos][1] == tag:
			return ParseRes.new(tokens[pos][0], pos + 1)
		else: return null
	func _to_string() -> String: return str("Reserved('", value, "')")

class Tag extends Parser:
	var tag: String
	func _init(t: String) -> void: tag = t
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		if pos < tokens.size() and tokens[pos][1] == tag: return ParseRes.new(tokens[pos][0], pos + 1)
		else: return null
	func _to_string() -> String: return str("Tag('", tag, "')")

class Concat extends Parser:
	var left: Parser
	var right: Parser
	func _init(l: Parser, r: Parser) -> void: left = l; right = r
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		var left_res := left.fn(tokens, pos)
		if left_res:
			var right_res := right.fn(tokens, left_res.pos)
			if right_res:
				var comb_val := [ left_res.value, right_res.value ]
				return ParseRes.new(comb_val, right_res.pos)
		return null
	func _to_string() -> String: return str("Concat(", left, ", ", right, ")")

class Alternate extends Parser:
	var left: Parser
	var right: Parser
	func _init(l: Parser, r: Parser) -> void: left = l; right = r
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		var left_res := left.fn(tokens, pos)
		if left_res: return left_res
		return right.fn(tokens, pos)
	func _to_string() -> String: return str("Alternate(", left, ", ", right, ")")

class Opt extends Parser:
	var parser: Parser
	func _init(p: Parser) -> void: parser = p
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		var res := parser.fn(tokens, pos)
		if res: return res
		return ParseRes.new(null, pos)
	func _to_string() -> String: return str("Opt(", parser, ")")

class Rep extends Parser:
	var parser: Parser
	func _init(p: Parser) -> void: parser = p
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		var results := []
		var res := parser.fn(tokens, pos)
		while res:
			results.append(res.value)
			pos = res.pos
			res = parser.fn(tokens, pos)
		return ParseRes.new(results, pos)
	func _to_string() -> String: return str("Rep(", parser, ")")

class Process extends Parser:
	var parser: Parser
	var function: Callable
	func _init(p: Parser, f: Callable) -> void: parser = p; function = f
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		var res := parser.fn(tokens, pos)
		if res:
			res.value = function.call(res.value)
			return res
		return null
	func _to_string() -> String: return str("Process(", parser, ")")

class Lazy extends Parser:
	var parser: Parser
	var parser_func: Callable
	func _init(f: Callable) -> void: parser = null; parser_func = f
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		if not parser: parser = parser_func.call()
		return parser.fn(tokens, pos)
	func _to_string() -> String: return str("Lazy(", parser, ")")

class Phrase extends Parser:
	var parser: Parser
	func _init(p: Parser) -> void: parser = p
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		var res := parser.fn(tokens, pos)
		return res if res and res.pos == tokens.size() else null
	func _to_string() -> String: return str("Phrase(", parser, ")")

# renamed from Exp
class Splice extends Parser:
	var parser: Parser
	var separator: Parser
	func _init(p: Parser, s: Parser) -> void: parser = p; separator = s
	func fn(tokens: Array[Array], pos: int) -> ParseRes:
		var res := [ parser.fn(tokens, pos) ] # put in an array so we can use it in a closure
		var process_next := func(parsed): return parsed[0].call(res[0].value, parsed[1])
		var next_parser := separator.concat(parser).process(process_next)
		var next_res = res[0]
		while next_res:
			next_res = next_parser.fn(tokens, res[0].pos)
			if next_res: res[0] = next_res
		return res[0]
	func _to_string() -> String: return str("Splice(", parser, " ", separator, ")")

### AST

class Exp:
	func eval(_env: Dictionary):
		return null

class Aexp extends Exp: # Arithmetic
	pass

class IntAexp extends Aexp:
	var val: int
	func _init(i: int) -> void: val = i
	func _to_string() -> String: return str("IntAexp(", val, ")")
	func eval(_env: Dictionary): return val

class StringAexp extends Aexp:
	var val: String
	func _init(s: String) -> void: val = s.substr(1, s.length() - 2).c_unescape() # removing the quotation marks
	func _to_string() -> String: return str("StringAexp(", val, ")")
	func eval(_env: Dictionary): return val

class VarAexp extends Aexp:
	var name: String
	func _init(n: String) -> void: name = n
	func _to_string() -> String: return str("VarAexp('", name, "')")
	func eval(env: Dictionary): return env[name] if env.has(name) else 0

class BinopAexp extends Aexp:
	var op: String
	var left: Aexp
	var right: Aexp
	func _init(o: String, l: Aexp, r: Aexp) -> void: op = o; left = l; right = r
	func _to_string() -> String: return str("BinopAexp('", op, "', ", left, ", ", right, ")")
	func eval(env: Dictionary):
		var l = left.eval(env)
		var r = right.eval(env)
		match op:
			"+": return l + r
			"-": return l - r
			"*": return l * r
			"/": return l / r
		printerr("Unknown op ", op)
		return null

class Bexp extends Exp: # Boolean
	pass

class BoolBexp extends Bexp:
	var val: bool
	func _init(b: bool) -> void: val = b
	func _to_string() -> String: return str("BoolAexp(", val, ")")
	func eval(_env: Dictionary): return val

class RelopBexp extends Bexp:
	var op: String
	var left: Aexp
	var right: Aexp
	func _init(o: String, l: Aexp, r: Aexp) -> void: op = o; left = l; right = r
	func _to_string() -> String: return str("RelopBexp('", op, "', ", left, ", ", right, ")")
	func eval(env: Dictionary):
		match op:
			"<": return left.eval(env) < right.eval(env)
			"<=": return left.eval(env) <= right.eval(env)
			">": return left.eval(env) > right.eval(env)
			">=": return left.eval(env) >= right.eval(env)
			"==": return left.eval(env) == right.eval(env)
			"!=": return left.eval(env) != right.eval(env)
		printerr("Unknown op ", op)
		return null

class AndBexp extends Bexp:
	var left: Bexp
	var right: Bexp
	func _init(l: Bexp, r: Bexp) -> void: left = l; right = r
	func _to_string() -> String: return str("AndBexp(", left, ", ", right, ")")
	func eval(env: Dictionary): return left.eval(env) and right.eval(env)

class OrBexp extends Bexp:
	var left: Bexp
	var right: Bexp
	func _init(l: Bexp, r: Bexp) -> void: left = l; right = r
	func _to_string() -> String: return str("OrBexp(", left, ", ", right, ")")
	func eval(env: Dictionary): return left.eval(env) or right.eval(env)

class NotBexp extends Bexp:
	var bexp: Bexp
	func _init(b: Bexp) -> void: bexp = b
	func _to_string() -> String: return str("NotBexp(", bexp, ")")
	func eval(env: Dictionary): return not bexp.eval(env)

class AssignExp extends Exp:
	var name: String
	var express: Exp
	func _init(n: String, e: Exp) -> void: name = n; express = e
	func _to_string() -> String: return str("AssignExp(", name, ", ", express, ")")
	func eval(env: Dictionary):
		var val = express.eval(env)
		env[name] = val
		return val

class CompoundExp extends Exp:
	var first: Exp
	var second: Exp
	func _init(f: Exp, s: Exp) -> void: first = f; second = s
	func _to_string() -> String: return str("CompoundExp(", first, ", ", second, ")")
	func eval(env: Dictionary):
		first.eval(env)
		return second.eval(env)

class IfExp extends Exp:
	var condition: Bexp
	var true_exp: Exp
	var false_exp: Exp
	func _init(c: Bexp, t: Exp, f: Exp) -> void: condition = c; true_exp = t; false_exp = f
	func _to_string() -> String: return str("IfExp(", condition, ", ", true_exp, ", ", false_exp, ")")
	func eval(env: Dictionary):
		if condition.eval(env):
			return true_exp.eval(env)
		elif false_exp:
			return false_exp.eval(env)
		return null

class WhileExp extends Exp:
	var condition: Bexp
	var body: Exp
	func _init(c: Bexp, b: Exp) -> void: condition = c; body = b
	func _to_string() -> String: return str("WhileExp(", condition, ", ", body, ")")
	func eval(env: Dictionary):
		var val = null
		while condition.eval(env):
			val = body.eval(env)
		return val

### PARSER

func _keyword(kw: String) -> Parser:
	return Reserved.new(kw, _RESERVED)

var _id := Tag.new(_ID)
var _num := Tag.new(_INT).process(func(i) -> int: return int(i))
var _string := Tag.new(_STRING).process(func(s) -> String: return str(s))
var _bool := Tag.new(_BOOL).process(func(b) -> bool: if b is String: return b == "true" else: return bool(b))

func _aexp_value() -> Parser:
	return _num.process(func(i) -> Aexp: return IntAexp.new(i)) \
		.alternate(_string.process(func(s) -> Aexp: return StringAexp.new(s))) \
		.alternate(_id.process(func(v) -> Aexp: return VarAexp.new(v)))

func _process_group(parsed):
	return parsed[0][1]

func _aexp_group() -> Parser:
	return _keyword("(").concat(Lazy.new(_aexp)).concat(_keyword(")")).process(_process_group)

func _aexp_term() -> Parser:
	return _aexp_value().alternate(_aexp_group())
	
func _process_binop(op: String) -> Callable:
	return func(l: Aexp, r: Aexp) -> Aexp: return BinopAexp.new(op, l, r)

func _any_operator_in_list(ops) -> Parser:
	var op_parsers = ops.map(func(o: String) -> Parser: return _keyword(o))
	return op_parsers.reduce(func(l: Parser, r: Parser) -> Parser: return l.alternate(r))

func _precedence(value_parser: Parser, precedence_levels: Array[Array], combine: Callable) -> Parser:
	var op_parser := func(precedence_level) -> Parser:
		return _any_operator_in_list(precedence_level).process(combine)
	var parser := value_parser.splice(op_parser.call(precedence_levels[0]))
	for precedence_level in precedence_levels.slice(1):
		parser = parser.splice(op_parser.call(precedence_level))
	return parser

func _aexp() -> Parser:
	return _precedence(_aexp_term(), _aexp_precedence_levels, _process_binop)

func _process_relop(parsed) -> Bexp:
	return RelopBexp.new(parsed[0][1], parsed[0][0], parsed[1])
	#((left, op), right) = parsed \\ #return RelopBexp(op, left, right)

func _bexp_relop() -> Parser:
	return _aexp().concat(_any_operator_in_list(_relops)).concat(_aexp()).process(_process_relop)

func _bexp_not() -> Parser:
	return _keyword("not").concat(Lazy.new(_bexp_term)).process(func(parsed) -> Bexp: return NotBexp.new(parsed[1]))

func _bexp_value() -> Parser:
	return _bool.process(func(b) -> Bexp: return BoolBexp.new(b))
	
func _bexp_group() -> Parser:
	return _keyword("(").concat(Lazy.new(_bexp)).concat(_keyword(")")).process(_process_group)

func _bexp_term():
	return _bexp_value().alternate(_bexp_not()).alternate(_bexp_relop()).alternate(_bexp_group())

func _process_logic(op: String) -> Callable:
	if op == "and": return func(l, r) -> Bexp: return AndBexp.new(l, r)
	if op == "or": return func(l, r) -> Bexp: return OrBexp.new(l, r)
	printerr("Unknown logic operator: ", op)
	return Callable()

func _bexp() -> Parser:
	return _precedence(_bexp_term(), _bexp_precedence_levels, _process_logic)

func _assign_exp() -> Parser:
	var process := func(parsed) -> Exp:
		return AssignExp.new(parsed[0][0], parsed[1])
	#return _id.concat(_keyword("=")).concat(_aexp()).process(process)
	return _id.concat(_keyword("=")).concat(Lazy.new(_exp)).process(process)

func _exp_list() -> Parser:
	var separator = _keyword(";").process(func(_x): return func(l, r): return CompoundExp.new(l, r))
	return Splice.new(_exp(), separator)

func _if_exp() -> Parser:
	var process := func(parsed) -> Exp:
		# (((((_, condition), _), true_exp), false_parsed), _) = parsed
		var false_exp = parsed[0][1][1] if parsed[0][1] else null
		return IfExp.new(parsed[0][0][0][0][1], parsed[0][0][1], false_exp)
	return _keyword("if").concat(_bexp()).concat(_keyword("then")).concat(Lazy.new(_exp_list)) \
		.concat(Opt.new(_keyword("else").concat(Lazy.new(_exp_list)))) \
		.concat(_keyword("end")).process(process)

func _while_exp() -> Parser:
	var process := func(parsed) -> Exp:
		return WhileExp.new(parsed[0][0][0][1], parsed[0][1])
	return _keyword("while").concat(_bexp()).concat(_keyword("do")).concat(Lazy.new(_exp_list)) \
		.concat(_keyword("end")).process(process)

func _exp() -> Parser:
	return _assign_exp().alternate(_if_exp()).alternate(_while_exp()).alternate(_bexp()).alternate(_aexp())
