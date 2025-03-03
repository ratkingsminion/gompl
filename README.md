# Gompl

*Gompl is based on IMP, a [tiny tutorial language](https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-) by Jay Conrod. I needed a small scripting language for our current game project, so I ported Conrod's IMP to GDScript, extended it a bit and named it Gimpl. After a while I wasn't totally satisfied with the outcome, so I took Jay Conrod's advice and ditched the combinators approach and now use recursive descent parsing as described in [Crafting Interpreters](https://craftinginterpreters.com).*

---

Gompl uses = for assignment and == for comparison.

Everything is an expression, so you can do things like `x = if y != 5 then 0 else 10 end`. No `;`s are necessary.

For "break" and "continue" use `stop` and `skip` in `while`-loops.

Use `elif`-`then` if you want an `if`-`then` with more than one condition.

Gompl also supports negative numbers (IMP didn't), strings and function calls. The functions are fed to the interpreter by setting a target Godot object whose methods are directly called by Gompl.

Example:

```GDScript
func some_method(p):
  print(p)

func _ready() -> void:
  var gompl := Gompl.new(self)
  var res = gompl.eval('
	x = 0 // variables should be initialised
	while x < 10 do
	  some_method(x) // be careful to use the correct amount and type of parameters
	  x = x + 1
	end')
  print(res)
```

`eval()` returns the value of the last evaluated expression, so in the example this would be the expression `x = x + 1` and the printed result will be 10.

Limitations:

* Sub-par error handling/messages (only shows current token, not line number)
* Only integers for numbers
* No functions
* No arrays
* All variables have global scope
* Only `while`-`do` exists, no for-loop
* Probably not the best performance
