# Gompl

*Gompl is based on IMP, a [tiny tutorial language](https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-) by Jay Conrod. I needed a small scripting language for our current game project, so I ported Conrod's IMP to GDScript, extended it a bit and named it Gimpl. After a while I wasn't totally satisfied with the outcome, so I took Jay Conrod's advice and ditched the combinators approach and now use recursive descent parsing as described in [Crafting Interpreters](https://craftinginterpreters.com).*

---

## Example

```GDScript
func some_method(p):
  print(p)

func _ready() -> void:
  var gompl := Gompl.new(self)
  var res = gompl.eval('
	// Gompl code begins here
	
	// variables should be initialised, otherwise they are "undefined"
	x = 0
	while x < 10 do
	  some_method(x)
	  x = x + 1
	end')
  print(res)
```

`eval()` returns the value of the last evaluated expression, so in the example this would be the expression `x = x + 1` and the printed result will be 10.

## Limitations

* No arrays
* All variables have global scope
* Only `while`-`do` exists, no for-loop
* Probably not the best performance

## Keywords

* and
* or
* not
* if
* then
* elif
* else
* end
* while
* do
* stop
* skip
* interrupt
* with
* function

## Notes

No semicolons or linebreaks are necessary.

Gompl uses `=` for assignment and `==` for comparison.

Instead of "break" and "continue" use `stop` and `skip` in `while` loops.

Everything is an expression, so you can do things like `x = if y != 5 then 0 else 10 end`. Be aware that in some cases the result can be `undefined`, e.g. when the `if` condition is false and there's no `else` clause. Another case is the result of a `while` loop that was stopped via `stop` without `with` modifier.

Gompl natively supports integers, floats, bools, strings and function calls. Outside functions are fed to the interpreter by setting a target Godot object whose methods are directly called by Gompl and/or by registering functions via Gompl's `register_func` method. Setting a target object will allow access to all of its methods, which might be undesirable.

Using `interrupt` will exit the script, but when providing a state `Dictionary` you can continue the execution. It's also possible to limit the amount of execution steps, and interrupting the script via `state["interrupt"] = true` (for example, inside a function called from Gompl).

All flow control keywords (`stop`, `skip` and `interrupt`) allow the modifier `with` with an expression afterwards, which is then the result of the loop.

Gompl functions do not (yet) allow any parameters, and they always return the result of the last expression in the body, though `stop` and `stop with <expression>` are allowed inside functions for a premature return. (`skip` is allowed too - it returns to the function's beginning, which might be an interesting side effect.)
