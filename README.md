# llvm-test

A small c-like compiler for mostly for learning the `llvm-hs` bindings.

The syntax looks as follows:

```
// a comment
factorial(x: int): int {
    ret: int = 1; // declaration
    while x > 0 {
        ret = ret * x;
        x = x - 1;
    }
    return ret;
}
```

