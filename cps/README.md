# lambda calculus

This project has a simple untyped lambda calculus that with literal integer extension.

The syntax in Antlr:

```antlr
grammar Lambda;

term:
  variable
  | application
  | lambda
  | INT
  ;

variable: IDENTIFIER ;
application: term term ;
lambda: '\' IDENTIFIER term ;

IDENTIFIER: [a-z]+ ;
INT: [0-9]+ ;
```

### CPS

- naive
- high order
- hybrid
- real language
