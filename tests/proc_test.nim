proc hello[A](s: seq[A], b: A) =
    s.add(b)

proc defined*(x: untyped): bool {.magic: "Defined", noSideEffect, compileTime.}

proc hello2(s: string, b: int): int =
    var s2 : seq[string] = @["hello"]
    s2.add(s)
    s2.hello($b)
    syntax_error;
    return s2.len
