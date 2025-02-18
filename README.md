# Gimpl

Gimpl is based on IMP, a tiny tutorial language by Jay Conrod: https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-

---

Gimpl uses = for assignment and == for comparison.

Everything is expressions, so you can do things like `x = if y != 5 then 0 else 10 end`.

Gimpl also supports strings and function calls. The functions are fed to the interpreter by setting a target Godot object whose methods are directly called by Gimpl.

Example:

```GDScript
func some_method(p):
  print(p)

func _ready() -> void:
  var gimpl := Gimpl.new(self)
  # variables are always initialised as 0
  gimpl.eval('
    while x < 10 do
      some_method(x);
      x = x + 1
    end')
```

`eval()` returns the value of the last evaluated expression, so in the example the result would be 10.

Limitations:

* Almost no error messages
* No comments allowed
* Lists of expressions need to be separated with `;`
* There is no `break` and `continue`
* Only `while`-`do`-`end` exists, no for-loop
* Booleans are limited, e.g. `true == true` does not work
* Probably sub-optimal performance
