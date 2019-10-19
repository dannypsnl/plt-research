module Lang (
  Expr(..)
  , Op(..)
) where

data Expr =
  Int Integer
  | Bool Bool
  | String String
  | Var String
  | Func String Expr
  | Application Expr Expr
  | Let String Expr Expr
  | Binary Op Expr Expr
  deriving (Show, Eq)

data Op =
  Add
  | Sub
  | Mul
  | Div
  | Equal
  | NotEqual deriving (Show, Eq)
