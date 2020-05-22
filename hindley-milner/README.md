# hindley milner

This is just a simple(and of course buggy) language for showing how to inference expression and get automatically type.

However, this technology is not really helpful for real programming job.

Even though I use Haskell, I mostly would give binding a nice type, since those messages from inferencer are usually hard to understand.

### Example

```racket
#lang s-exp "syntax.rkt"

(λ (a) "")
(λ () #t)
(λ (a b) 1)
```

TODO:

- let binding syntax
- list literal
- variable
- application
- automatically eval type and eval result
