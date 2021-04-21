strictly-positive
=================
Check strictly positive, rules:
1. endofunctors are positive
2. if `\Pi x:A B` is positive, then `A` is negative, `B` is positive
3. if `\Pi x:A B` is negative, then `A` is positive, `B` is negative
4. check constructor start from `positive?` is `#t`
5. `A -> B` is a shorthand of `\Pi x:A B` when `B` doesn't depend on `x`
