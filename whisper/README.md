# whisper

This project implementing a conversion checking algorithm, following commands are some demo

```shell
$ stack run test.tt
λ x. x
  :
𝕌 → 𝕌

$ stack run postulate.tt
suc zero
  :
Nat
```

## Surface syntax

### Data type

```
data D
| c₁ : T₁
| c₂ : T₂
;
c₁
```

### Postulate

```
postulate x : A;
u
```

### Let binding

```
let x : A = t;
u
```

### Pi type

```
(x : A) -> B
```

### Application

```
t u
```
