extends Node

###

func _ready() -> void:
	var gimpl := Gimpl.new()
	
	gimpl.eval("n = 5;
		p = 1;
		while n > 0 do
		  p = p * n;
		  n = n - 1
		end")
	
	gimpl.eval('test = "str";
		t = if test != "str" then 100 else 50 end;
		r = true;
		t - 50 > 0')
	
	for i in 20: await get_tree().process_frame
	get_tree().quit()
