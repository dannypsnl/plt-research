-- Ref to https://www.cs.bham.ac.uk/~mhe/HoTT-UF-in-Agda-Lecture-Notes/index.html
--
-- The option --without-K disables Streicher’s K axiom, which we don’t want for univalent mathematics.
-- The option --exact-split makes Agda to only accept definitions with the equality sign “=” that behave like so-called judgmental or definitional equalities.
-- The option --safe disables features that may make Agda inconsistent, such as --type-in-type, postulates and more.
{-# OPTIONS --without-K --exact-split --safe #-}

module mltt where

-- Universes
-- A universe 𝓤 (later we would use U in comment instead of this crazy literal) is a type of types.
--
-- To solve Russell's Paradox, one way was provide level of universes(NOTE: I cannot sure this can solve it exactly or make it harder to find out)
-- Like the following shows:
--     U0, U1, U2...
-- We would have many Universe
--
-- We make universe U0 is a type in the universe U1, U1 is a type in U2 and so on.
-- Express as:
--
--     U0 : U1
--     U1 : U2
--     U2 : U3
--     ...
--
-- Assumption: `U0 : U0` or any universe is in itself, or a smaller universe would make rise to a contradiction
--
-- For a given universe U, we denote by U⁺ for its successor universe, so if U is U0, U⁺ would be U1.
-- According to above, U would be a type in U⁺, `U : U⁺`

-- TODO: create Universes.agda which rename Level to Universe and provide misc
