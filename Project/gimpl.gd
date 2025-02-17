class_name Gimpl
extends RefCounted

const _RESERVED := "RESERVED"
const _INT := "INT"
const _ID := "ID"
const _TOKEN_EXPRESSIONS: Array[String] = [
	r"[ \n\t]+", "", r"#[^\n]*", "", # whitespaces
	r";", _RESERVED, # TODO
	r"\+", _RESERVED, r"-", _RESERVED, r"\*", _RESERVED, r"/", _RESERVED,
	r"<=", _RESERVED, r"<", _RESERVED, r">=", _RESERVED, r">", _RESERVED,
	r"==", _RESERVED, r"!=", _RESERVED,
	r"\=", _RESERVED, r"\(", _RESERVED, r"\)", _RESERVED,
	r"and", _RESERVED, r"or", _RESERVED, r"not", _RESERVED,
	r"if", _RESERVED, r"then", _RESERVED, r"else", _RESERVED,
	r"while", _RESERVED, r"do", _RESERVED, r"end", _RESERVED,
	r"[0-9]+", _INT,
	r"[A-Za-z_][A-Za-z0-9_]*", _ID
]

var phrase: Parser

###

func _init() -> void:
	phrase = Phrase.new(_stmt_list())

###

func test_lexer(code: String):
	var res := _lex(code, _TOKEN_EXPRESSIONS)
	print(res)

func test_parser(code: String) -> ParseRes:
	#print("!Phrase: ", phrase)
	var tokens := _lex(code, _TOKEN_EXPRESSIONS)
	#print("!Tokens: ", tokens)
	var res := _imp_parse(tokens)
	#print("!ParseRes: ", res)
	return res

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
	#print(token_exprs)
	while pos < code.length():
		var res: RegExMatch = null
		for tidx: int in range(0, token_exprs.size(), 2):
			var reg := RegEx.new()
			reg.compile(token_exprs[tidx])
			res = reg.search(code, pos)
			if res and res.get_start() == pos:
				if token_exprs[tidx + 1]: # has tag
					#print("FOUND [", token_exprs[tidx + 1], "] at ", pos, ": '", res.get_string(),"'")
					var token: Array[String] = [ res.get_string(), token_exprs[tidx + 1] ]
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
	func exp(other: Parser) -> Parser: # *
		return Exp.new(self, other)
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

class Exp extends Parser:
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
	func _to_string() -> String: return str("Exp(", parser, " ", separator, ")")

### AST

class Aexp: # Arithmetic
	func eval(_env: Dictionary):
		pass

class IntAexp extends Aexp:
	var val: int
	func _init(i: int) -> void: val = i
	func _to_string() -> String: return str("IntAexp(", val, ")")
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
		match op:
			"+": return left.eval(env) + right.eval(env)
			"-": return left.eval(env) - right.eval(env)
			"*": return left.eval(env) * right.eval(env)
			"/": return left.eval(env) / right.eval(env)
		printerr("Unknown op ", op)
		return null

class Bexp: # Boolean
	func eval(env: Dictionary):
		pass

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

class Statement:
	func eval(env: Dictionary):
		pass

class AssignStatement extends Statement:
	var name: String
	var aexp: Aexp
	func _init(n: String, a: Aexp) -> void: name = n; aexp = a
	func _to_string() -> String: return str("AssignStatement(", name, ", ", aexp, ")")
	func eval(env: Dictionary): env[name] = aexp.eval(env)

class CompoundStatement extends Statement:
	var first: Statement
	var second: Statement
	func _init(f: Statement, s: Statement) -> void: first = f; second = s
	func _to_string() -> String: return str("CompoundStatement(", first, ", ", second, ")")
	func eval(env: Dictionary): first.eval(env); second.eval(env)

class IfStatement extends Statement:
	var condition: Bexp
	var true_stmt: Statement
	var false_stmt: Statement
	func _init(c: Bexp, t: Statement, f: Statement) -> void: condition = c; true_stmt = t; false_stmt = f
	func _to_string() -> String: return str("IfStatement(", condition, ", ", true_stmt, ", ", false_stmt, ")")
	func eval(env: Dictionary):
		if condition.eval(env): true_stmt.eval(env)
		elif false_stmt: false_stmt.eval(env)

class WhileStatement extends Statement:
	var condition: Bexp
	var body: Statement
	func _init(c: Bexp, b: Statement) -> void: condition = c; body = b
	func _to_string() -> String: return str("WhileStatement(", condition, ", ", body, ")")
	func eval(env: Dictionary):
		while condition.eval(env):
			body.eval(env)

### PARSER

func _keyword(kw: String) -> Parser:
	return Reserved.new(kw, _RESERVED)

var _id := Tag.new(_ID)

var _num := Tag.new(_INT).process(func(i: String) -> int: return int(i))

func _aexp_value() -> Parser:
	return _num.process(func(i): return IntAexp.new(i)).alternate(_id.process(func(v): return VarAexp.new(v)))

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

const _aexp_precedence_levels: Array[Array] = [ ['*', '/'], ['+', '-'] ]

func _precedence(value_parser: Parser, precedence_levels: Array[Array], combine: Callable) -> Parser:
	var op_parser := func(precedence_level) -> Parser:
		return _any_operator_in_list(precedence_level).process(combine)
	var parser := value_parser.exp(op_parser.call(precedence_levels[0]))
	for precedence_level in precedence_levels.slice(1):
		parser = parser.exp(op_parser.call(precedence_level))
	return parser

func _aexp() -> Parser:
	return _precedence(_aexp_term(), _aexp_precedence_levels, _process_binop)

func _process_relop(parsed) -> Bexp:
	return RelopBexp.new(parsed[0][1], parsed[0][0], parsed[1])
	#((left, op), right) = parsed \\ #return RelopBexp(op, left, right)

func _bexp_relop() -> Parser:
	var relops: Array[String] = ['<', '<=', '>', '>=', '=', '!=']
	return _aexp().concat(_any_operator_in_list(relops)).concat(_aexp()).process(_process_relop)

func _bexp_not() -> Parser:
	return _keyword("not").concat(Lazy.new(_bexp_term)).process(func(parsed) -> Bexp: return NotBexp.new(parsed[1]))

func _bexp_group() -> Parser:
	return _keyword("(").concat(Lazy.new(_bexp)).concat(_keyword(")")).process(_process_group)

func _bexp_term():
	return _bexp_not().alternate(_bexp_relop()).alternate(_bexp_group())

const _bexp_precedence_levels: Array[Array] = [ ['and'], ['or'] ]

func _process_logic(op: String) -> Callable:
	if op == "and": return func(l, r) -> Bexp: return AndBexp.new(l, r)
	if op == "or": return func(l, r) -> Bexp: return OrBexp.new(l, r)
	printerr("Unknown logic operator: ", op)
	return Callable()

func _bexp() -> Parser:
	return _precedence(_bexp_term(), _bexp_precedence_levels, _process_logic)

func _assign_stmt() -> Parser:
	var process := func(parsed) -> Statement:
		return AssignStatement.new(parsed[0][0], parsed[1])
	return _id.concat(_keyword("=")).concat(_aexp()).process(process)

func _stmt_list() -> Parser:
	var separator = _keyword(";").process(func(_x): return func(l, r): return CompoundStatement.new(l, r))
	return Exp.new(_stmt(), separator)

func _if_stmt() -> Parser:
	var process := func(parsed) -> Statement:
		# (((((_, condition), _), true_stmt), false_parsed), _) = parsed
		var false_stmt = parsed[0][1][1] if parsed[0][1] else null
		return IfStatement.new(parsed[0][0][0][0][1], parsed[0][0][1], false_stmt)
	return _keyword("if").concat(_bexp()).concat(_keyword("then")).concat(Lazy.new(_stmt_list)) \
		.concat(Opt.new(_keyword("else").concat(Lazy.new(_stmt_list)))) \
		.concat(_keyword("end")).process(process)

func _while_stmt() -> Parser:
	var process := func(parsed) -> Statement:
		return WhileStatement.new(parsed[0][0][0][1], parsed[0][1])
	return _keyword("while").concat(_bexp()).concat(_keyword("do")).concat(Lazy.new(_stmt_list)) \
		.concat(_keyword("end")).process(process)

func _stmt() -> Parser:
	return _assign_stmt().alternate(_if_stmt()).alternate(_while_stmt())
