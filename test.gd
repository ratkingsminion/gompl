extends Node

###

func func_1():
	print("Function call test 1 - called a function without parameters")

func func_2(a, b, c = "optional param"):
	prints("Function call test 2 -", a, b, c)
	return "return value from func_2"

func print(p): print(p)

###

func _ready() -> void:
	var g := Gompl.new(self)
	var res
	
	# test calling GDScript functions
	res = g.eval('
		ifif = 2 // keywords can be part of the identifier names
		func_1()
		func_2("foo", 2,)
		// functions need to have the correct parameter count and types,
		// otherwise you get GDScript errors inside the Debugger
		// (instead of just Gompl errors)
	')
	print("RESULT 1: ", res) # "return value from func_2"
	
	# test factorial code
	res = g.eval('
		n = 5 // no ; needed
		p = 1
		while n > 0 do
		  p = p * n
		  n = n - 1
		end
		p // the last expression is the result of the eval() call
	')
	print("RESULT 2: ", res) # 120
	
	# test custom env Dictionary, and some assignments
	var env = {}
	res = g.eval('
		test = "str"
		t = if test != "str" then 100 else 50 end // if-then-else and while-do can be used as expressions
		r = s = -5 // assignments are expressions too
		w = false
		r = undefined // r will be removed from env
		t - 50 == 0
	', env)
	print("RESULT 3: ", res) # true
	print("-> WITH ENVIRONMENT: ", env)
	
	# test undefined (== null in GDScript)
	res = g.eval('
		y = if 1 + 1 == 3 then "y is undefined because this if-expression returns null" end
		if y == undefined then print("y is not defined") end
		y // will return undefined
	')
	print("RESULT 4: ", res) # undefined
	
	# test skip and stop
	res = g.eval('
		x = -1
		while x < 10 do
			x = x + 1
			if x == 3 then skip end
			if x == 6 then stop end
			print(x)
		end
		x
	')
	print("RESULT 5: ", res) # 6
	
	# Wait a bit before closing
	for i in 20: await get_tree().process_frame
	get_tree().quit()
