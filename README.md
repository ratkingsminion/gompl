# Gompl

A simple scripting language written in GDScript with focus on safe usage (no Godot errors) for embedding. Used in [I Made A Game For You](https://medienzentrale.itch.io/a-game-for-you) and [Mops & Mobs](https://store.steampowered.com/app/2851050/).

---

## Example (GDScript with embedded Gompl code)

```GDScript
func some_method(p):
	print(p)

func _ready() -> void:
	var gompl := Gompl.new(self)
	var res = gompl.eval('
	  // Gompl code begins here!
	  x = 0 // variables should be initialised, otherwise they are "undefined"
	  while x < 10 do
	    some_method(x)
	   x = x + 1
	  end')
	print(res)
```

`eval()` returns the value of the last evaluated expression, so in the example this would be the expression `x = x + 1` and the printed result will be 10.

## Limitations

* All variables have global scope
* Internal functions don't support arguments
* Only `while`-`do` exists, no for-loop or other loop constructs
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
* array
* dictionary
* from

## Notes

No semicolons or linebreaks are necessary.

Use `=` for assignment and `==` for comparison.

Instead of "break" and "continue", write `stop` and `skip` in `while` loops.

Everything is an expression, so you can do things like `x = if y > 10 then 2 elif y > 5 then 1 else 0 end`. Be aware that in some cases the result can be `undefined`, e.g. when the `if` condition is false and there's no `else` clause. Another case is the result of a `while` loop that was stopped via `stop` without `with` modifier.

Gompl natively supports integers, floats, bools, strings, arrays, dictionaries and function calls. Outside functions are fed to the interpreter by setting a target Godot object whose methods are directly called by Gompl and/or by registering functions via Gompl's `register_func` method. Setting a target object will allow access to all of its methods, which might be undesirable.

Using `interrupt` will exit the script, but when providing a state `Dictionary` you can continue the execution. It's also possible to limit the amount of execution steps, and interrupting the script via `state["interrupt"] = true` (i.e. inside a GDScript function called from Gompl).

All flow control keywords (`stop`, `skip` and `interrupt`) allow the modifier `with` with an expression afterwards, which is then the result of the loop.

Gompl functions do not allow any parameters, and they return the result of the last expression in the body, though `stop` and `stop with <expression>` are allowed inside functions for a premature return. (`skip` is allowed too - it returns to the function's beginning, which might be an interesting side effect.)

Arrays are always untyped and initialised like this: `a = array(1, 2, 3)`, array access uses square brackets: `foo = a[0]`, `a[1] = "bar"`. Most methods of Godot's arrays are supported, apart from those using Callables, e.g. filter(), and all *_custom() methods. The methods are called via `a.method(<parameters>)`, and most of them return the array again. This way you can use currying, e.g. `a = array(4, 3, 2).append(1).sort()` (`a` will be `[ 1, 2, 3, 4 ]`).

Dictionaries are also supported: `d = dictionary("a": 1, "b": 2, "c": 3)`. The same rules as to arrays apply, and no method regarding types are supported. `set(entry)` returns the dictionary itself instead of true/false like in Godot; `entry` can be a key-value pair (`"foo": 5`), or anything else, which then creates the key with undefined value. This also works during `dictionary()` initialisation.

In order to iterate over an array or a dictionary you can use `from`:

```Lua
d = dictionary("name": "Klapauzius", "age": 10000, "weight": 123.4)
while key from d do
	print("Key: " + key + " ... Value: " + d[key])
	// -- not allowed: key = <value>
	// -- not recommended: d.erase(key)
end
```

`from` also works with numbers. Be aware that the right side expression of `from` is evaluated on every iteration, which leads to unexpected behaviour when the expression's result changes. To clear an iterator and make it assignable again, call `i from undefined`.

## History

Gompl was based on IMP, a [tiny tutorial language](https://jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-) by Jay Conrod. I needed a small scripting language for our game project, so I ported Conrod's IMP to GDScript, extended it a bit and named it Gimpl. After a while I wasn't totally satisfied with the outcome, so I took Jay Conrod's advice and ditched the combinators approach and now use recursive descent parsing as described in [Crafting Interpreters](https://craftinginterpreters.com).
