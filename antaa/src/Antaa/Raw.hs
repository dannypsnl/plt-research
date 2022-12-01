module Antaa.Raw
  ( Name,
    Raw (..),
  )
where

import Text.Megaparsec

type Name = String

data Raw
  = RVar Name -- x
  | RLam Name Raw -- \x. t
  | RApp Raw Raw -- t u
  | RU -- U
  | RPi Name Raw Raw -- (x : A) -> B
  | RLet Name Raw Raw Raw -- let x : A = t; u
  | RSrcPos SourcePos Raw -- source position for error reporting
  | RHole -- _
  deriving (Show)
