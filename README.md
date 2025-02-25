# Gompl

*Gompl is based on IMP, a [tiny tutorial language](https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-) by Jay Conrod. I needed a small scripting language for our current game project, so I ported Conrod's IMP to GDScript, extended it a bit and named it Gimpl. After a while I wasn't totally satisfied with the outcome, so I took Jay Conrod's advice and ditched the combinators approach and now use recursive descent parsing as described in https://craftinginterpreters.com.*

---

Gompl uses = for assignment and == for comparison.

Everything is an expression, so you can do things like `x = if y != 5 then 0 else 10 end`.

Gompl also supports negative numbers (IMP didn't), strings and function calls. The functions are fed to the interpreter by setting a target Godot object whose methods are directly called by Gompl.

Example:

```GDScript
func some_method(p):
  print(p)

func _ready() -> void:
  var gimpl := Gompl.new(self)
  # variables are always initialised as 0
  var res = gompl.eval('
    while x < 10 do
      some_method(x) // be careful to use the corrent parameters
      x = x + 1
    end')
  print(res)
```

`eval()` returns the value of the last evaluated expression, so in the example the result would be 10.

Limitations:

* Probably sub-par error handling/messages
* Comments via `//` only
* Only `while`-`do` exists, no for-loop
* An `if`-`then`-`else` with more than one `else` has to be nested
* There is no `break` and `continue` for loops
* Probably not the best performance
