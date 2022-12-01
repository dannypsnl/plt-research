module Syntax
  ( Name,
    Ty,
    Tm (..),
    Raw,
  )
where

import Text.Megaparsec

-- syntax
--------------------------------------------------------------------------------

type Name = String

type Ty = Tm

type Raw = Tm

data Tm
  = Var Name -- x
  | Lam Name Tm -- \x. t
  | App Tm Tm -- t u
  | U -- U
  | Pi Name Ty Ty -- (x : A) -> B
  | Let Name Ty Tm Tm -- let x : A = t; u
  | Postulate Name Ty Tm -- postulate x : A; u
  | SrcPos SourcePos Tm -- source position for error reporting

-- printing
--------------------------------------------------------------------------------

-- printing precedences
atomp, appp, pip, letp :: Int
atomp = 3 :: Int -- U, var
appp = 2 :: Int -- application
pip = 1 :: Int -- pi
letp = 0 :: Int -- let, lambda

-- | Wrap in parens if expression precedence is lower than
--   enclosing expression precedence.
par :: Int -> Int -> ShowS -> ShowS
par p p' = showParen (p' < p)

prettyTm :: Int -> Tm -> ShowS
prettyTm = go
  where
    piBind x a =
      showParen True ((x ++) . (" : " ++) . go letp a)

    go :: Int -> Tm -> ShowS
    go p = \case
      Var x -> (x ++)
      App t u -> par p appp $ go appp t . (' ' :) . go atomp u
      Lam x t -> par p letp $ ("λ " ++) . (x ++) . goLam t
        where
          goLam (Lam x' t') = (' ' :) . (x' ++) . goLam t'
          goLam t' = (". " ++) . go letp t'
      U -> ("𝕌" ++)
      Pi "_" a b -> par p pip $ go appp a . (" → " ++) . go pip b
      Pi x a b -> par p pip $ piBind x a . goPi b
        where
          goPi (Pi x' a' b') | x' /= "_" = piBind x' a' . goPi b'
          goPi b' = (" → " ++) . go pip b'
      Let x a t u ->
        par p letp $
          ("let " ++)
            . (x ++)
            . (" : " ++)
            . go letp a
            . ("\n    = " ++)
            . go letp t
            . ("\n;\n" ++)
            . go letp u
      Postulate x a u ->
        par p letp $
          ("postulate " ++)
            . (x ++)
            . (" : " ++)
            . go letp a
            . ("\n;\n" ++)
            . go letp u
      SrcPos _ t -> go p t

instance Show Tm where showsPrec = prettyTm
