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
	code_editor.text = code
	code_compile.button_down.connect(on_code_compile)
	on_code_compile()

func _process(_delta: float) -> void:
	if not gompl.err:
		# the code is compiled every frame, this is wasteful, but okay for this small example
		gompl.eval(code, null, 5000, state)
		code_error.text = gompl.err if gompl.err else ""

###

func on_code_compile() -> void:
	state.clear()
	code = code_editor.text
	gompl.err = ""
	transform = orig_transform
