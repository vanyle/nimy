when true:
    type A* = int

    when false:
        type AB* = float
    else:
        type AB* = bool
else:
    type A* = string

when 1 == 2:
    type B* = float
elif 1 == 3:
    type B* = int
else:
    type C* = bool
