extends Node


# Called when the node enters the scene tree for the first time.
func _ready() -> void:
	var code := "n = 5;
	p = 1;
	while n > 0 do
	  p = p * n;
	  n = n - 1
	end"
	var gimpl := Gimpl.new()
	gimpl.eval(code)
	
	for i in 20: await get_tree().process_frame
	get_tree().quit()
