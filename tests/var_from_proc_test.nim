
proc my_fn(x: int): float = discard
proc my_fn(x: string): seq[string] = discard

proc `*`(a: seq[string], b: seq[string]): int = discard

# var1 is my_fn(string), so it is a seq[string]
var var1 = my_fn("hello")

# var2 is my_fn(int), so it is a float
var var2 = my_fn(3)

# var3 is seq[string] * seq[string], so it is an int
var var3 = @["aa"] * @["bb"]

# var4 is my_fn(int), so it is a float
var var4 = my_fn(var3)
