# whisper

This project tests ability of typed/racket by implementing conversion checking algorithm, you can run `racket main.rkt` to get the following result.

```shell
type of term:

let id : (A : 𝕌) → A → A = λA.λx.x;
id 𝕌

is

𝕌 → 𝕌
```
