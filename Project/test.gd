extends Node

###

func func_call_1():
	print("TEST 1")

func func_call_2(a, b, c = "hallo"):
	printt(a, b, c)
	return "return!"

func _ready() -> void:
	var gimpl := Gimpl.new(self)
	
	gimpl.eval('
		func_call_1();
		func_call_2("foo", 2)')
	
	gimpl.eval("
		n = 5;
		p = 1;
		while n > 0 do
		  p = p * n;
		  n = n - 1
		end")
	
	gimpl.eval('
		test = "str";
		x = x + 5;
		t = if test != "str" then 100 else 50 end;
		r = s = 5;
		w = false;
		t - 50 > 0')
	
	for i in 20: await get_tree().process_frame
	get_tree().quit()
