extends Sprite2D

@export_multiline var code := "while true do\n\tdt = get_process_delta_time()\n\trotate(4 * dt)\n\t// move_local_x(5)\n\tinterrupt\nend"
@export var code_editor: TextEdit
@export var code_compile: Button
@export var code_error: Label

var state := {}

@onready var orig_transform := self.transform
@onready var gompl := Gompl.new(self)

###

func _ready() -> void:
	gompl.register_func("print", func(a): code_error.text = str(a), [ Gompl.T_ANY ])
	gompl.register_func("rnd", func(): return randf())
	gompl.register_func("mouse_pos", func() -> Vector2: return get_viewport().get_mouse_position())
	gompl.register_func("v2", func(x: float, y: float) -> Vector2: return Vector2(x, y), [ Gompl.T_NUMBER, Gompl.T_NUMBER ])
	
	code_editor.text = code
	code_compile.button_down.connect(on_code_compile)
	on_code_compile()

func _process(_delta: float) -> void:
	if not gompl.err:
		# the code is compiled every frame, this is wasteful, but okay for this small example
		gompl.eval(code, null, 5000, state)
		if gompl.err: code_error.text = gompl.err
		#gompl.debug_printing = false

###

func on_code_compile() -> void:
	code_error.text = ""
	#gompl.debug_printing = true
	state.clear()
	code = code_editor.text
	gompl.err = ""
	transform = orig_transform
