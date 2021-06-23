# llvm-test

A small compiler for a c-like language, mostly for learning the `llvm-hs`
bindings and imperative language typechecking.

The syntax looks as follows:

```
// a comment
factorial(x: int): int {
    ret: int = 1; // declaration
    while x > 0 {
        ret = ret * x; // no *=, sorry
        x = x - 1;
    }
    return ret;
}
```

## What's Q stand for?

A few AST types (`QExpr`, `QStmt`, etc) are prefixed with Q. It doesn't stand for anything (yet!).
