# Order of NINE digit

The question is: which order of all nine digit in 10-based number system, first two digits mod 2 is 0, 3 digits mod 3 is 0, and so on till 9.

`solve.smt` file describe question and give answer.

To run this file, must install z3 solver. `nix-env -i z3` is how I install it.

```
z3 -smt2 solve.smt
```
