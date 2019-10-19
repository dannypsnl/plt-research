# HM-lang

This is just a simple(and of course buggy) language for showing how to inference expression and get automatically type.

However, this technology is not really helpful for real programming job.

Even though I use Haskell, I mostly would give binding a nice type, since those messages from inferencer are usually hard to understand.

### Some examples

```
$ hm-lang "1"
int
$ hm-lang "1 /= 2"
bool
$ hm-lang "let f = fn x = x in f"
a -> a
$ hm-lang "let f = fn x = x in call f false"
bool
```
