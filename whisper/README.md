# whisper

This project implementing a conversion checking algorithm, you can run `stack run test.tt` to get the following result.

```shell
type of term:

let id : (A : 𝕌) → A → A = λA.λx.x;
id 𝕌

is

𝕌 → 𝕌
```
