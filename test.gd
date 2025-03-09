extends Node

###

func func_1():
	print("Function call test 1 - called a function without parameters")

func func_2(a, b, c := "optional param"):
	prints("Function call test 2 -", a, b, c)
	return "return value from func_2"

func print(p):
	print("Script prints '", p, "'")

###

func _ready() -> void:
	var g := Gompl.new(self)
	var res
	
	# test calling GDScript functions
	res = g.eval('
		ifif = 2 + 2 * 3 // keywords can be part of the identifier names
		func_1()
		func_2("foo", ifif)
		// functions need to have the correct parameter count and types,
		// otherwise you get GDScript errors inside the Debugger
		// (instead of just Gompl errors)
	')
	print("RESULT 1: ", res) # "return value from func_2"
	assert(res == "return value from func_2", "Result 1 wrong")
	
	# test factorial code
	res = g.eval('
		n = 5 // no ; needed, or even line-breaks
		p = 1
		while n > 0 do
		  p = p * n
		  n = n - 1
		end
		p // the last expression is the result of the eval() call
	')
	print("RESULT 2: ", res) # 120
	assert(res == 120, "Result 2 wrong")
	
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
	assert(res == true, "Result 3 wrong")
	
	# test undefined (similar to null in GDScript)
	res = g.eval('
		y = if 1 + 1 == 3 then "y is undefined because this if-expression returns null" end
		if y == undefined then print("y is not defined") end
		y // will return undefined
	')
	print("RESULT 4: ", res) # undefined
	assert(res == Gompl.undefined, "Result 4 wrong")
	
	# test conditions, skip and stop
	res = g.eval('
		x = -1
		while x < 10 do
			x = x + 1
			if x == 3 then
				print("no three for thee")
				skip
			elif x == 6 then
				stop
			end
			print(x)
		end
		x
	')
	print("RESULT 5: ", res) # 6
	assert(res == 6, "Result 5 wrong")
	
	# test string stuff
	res = g.eval('
		print("hello world" - "lo ") // subtracting removes the word(s)
		print("hello " * 3 + "world") // multiplying repeats the word
		"number test: " + 3.141 + " " + 1000
	')
	print("RESULT 6: ", res) # number test: 3.141 1000
	assert(res == "number test: 3.141 1000", "Result 6 wrong")
	
	# test endless loop and max steps of code execution
	var max_steps := 200
	var state = {}
	for i in 10:
		# this compiles the code again on every step, which is wasteful
		# better use g.run() instead
		g.eval('
			x = 0
			while true do
				x = x + 1
				if x > 100 then interrupt end // premature script exit
			end', null, max_steps, state)
		print("value of X on frame ", i, ": ", state["env"]["x"], " after ", state["steps"], " steps")
		await get_tree().process_frame
	
	# done, results in Output
	get_tree().quit()
