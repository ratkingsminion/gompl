extends Sprite2D

@export_multiline var code := "counter = 0\nspeed = array(-2, 5, 20).pick_random()\nwhile true do\n\tdt = get_process_delta_time()\n\trotate(speed * dt)\n\t// move_local_x(5)\n\tprint(counter = counter + 1)\n\tinterrupt\nend"
@export var code_editor: TextEdit
@export var code_compile: Button
@export var code_error: Label
@export var maxsteps_edit: TextEdit
@export var looping_toggle: CheckButton

var state := {}
var maxsteps := 2000
var is_looping := true
var executing := true

@onready var orig_transform := self.transform
@onready var gompl := Gompl.new(self)

###

func _ready() -> void:
	gompl.register_func("print", func(a): code_error.text += str(a, "\n"), [ Gompl.T_ANY ])
	gompl.register_func("rnd", func(): return randf())
	gompl.register_func("mouse_pos", func() -> Vector2: return get_viewport().get_mouse_position())
	gompl.register_func("v2", func(x: float, y: float) -> Vector2: return Vector2(x, y), [ Gompl.T_NUMBER, Gompl.T_NUMBER ])
	
	code_editor.text = code
	code_compile.button_down.connect(on_code_compile)
	maxsteps_edit.text_changed.connect(on_maxsteps_edit)
	maxsteps_edit.text = str(maxsteps)
	looping_toggle.toggled.connect(on_looping_toggled)
	on_code_compile()

func _process(_delta: float) -> void:
	if executing:
		code_error.text = ""
		if not gompl.err:
			# the code is compiled every frame, this is wasteful, but okay for this small example
			gompl.eval(code, null, state, maxsteps)
			if gompl.err: code_error.text = gompl.err
			#gompl.debug_printing = false
	if not is_looping:
		executing = false

###

func on_code_compile() -> void:
	code_error.text = ""
	#gompl.debug_printing = true
	state.clear()
	code = code_editor.text
	gompl.err = ""
	transform = orig_transform
	executing = true

func on_maxsteps_edit() -> void:
	maxsteps = clampi(int(maxsteps_edit.text), 1, 5000)
	maxsteps_edit.text = str(maxsteps)

func on_looping_toggled(toggled_on: bool) -> void:
	is_looping = toggled_on
	if toggled_on: executing = true
