# rapier

[![Build Status](https://travis-ci.org/dannypsnl/rapier.svg?branch=master)](https://travis-ci.org/dannypsnl/rapier)
[![Build status](https://ci.appveyor.com/api/projects/status/6c7edyqe22ba9cc4?svg=true)](https://ci.appveyor.com/project/dannypsnl/rapier)

This is a toy language compiler I want to using on teaching

## Lexer

What's a finite automata? It's a machine can change state & find out a target seq can matched the accepted state or not.

For example, if we have a string, and we want to know it is "cat" or not. We can have a finite automata like:

```
S0 -> S1 -> S2 -> S3
   c     a     t
```

That S3 is accepted state, we say the string matched!

This is a character-by-character algorithm. But in real world, we want to regonize more complex pattern.

We want to know a string is a digit? That something meaningful in higher level language.

For example a number regonizer can be

```
S0 -> S1 -> S2 -> ... // infinite
  0..9  0..9
```

We can find out that both S1, S2 and all following states is accepted and contains the same pattern `0..9`.
So we can have a new graph like:

```
S0 -> S1 --
  0..9 ^   \
       |   | 0..9
       ---/
```

And S1 is accepted state.

Now we can have math description a finite automata

- `S` represents all states, includes error state(not matched)
- `S0` represents beginning state
- `∑` represents all edge labels
- `∂` represents all state-to-state function
- `Sa` represents accepted state

We can have an easy algorithm to describing finite automata

```rust
fn ∂(now_state: int, label: char) -> int {
    return ∂s[now_state][label]() // a one of ∂ function return a new state
}

struct lexer {
    state: int
}

impl lexer {
    fn lex(&mut self, source: String) -> bool {
        self.state = s0;
        while !in_sa(self.state) && self.state != se {
            self.state = ∂(self.state)
        }
        if in_sa(self.state) {
            return true
        }
        return false
    }
}
```
