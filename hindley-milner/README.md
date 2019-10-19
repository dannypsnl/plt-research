# HM-lang

This is just a simple(and of course buggy) language for showing how to inference expression and get automatically type.

However, this technology is not really helpful for real programming job.

Even though I use Haskell, I mostly would give binding a nice type, since those messages from inferencer are usually hard to understand.

### Some examples

```
$ hm-lang "1"
TInt
$ hm-lang "1 /= 2"
TBool
$ hm-lang "let f = fn x = x in f"
TArrow (TypeVar "a") (TypeVar "a")
$ hm-lang "let f = fn x = x in call f 1"
TInt
```
