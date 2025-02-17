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
		if test != "str" then
			p = 100
		else
			p = 50
		end')
	
	for i in 20: await get_tree().process_frame
	get_tree().quit()
