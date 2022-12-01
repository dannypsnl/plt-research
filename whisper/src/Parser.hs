module Parser
  ( pSrc,
  )
where

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Functor
import Data.Void
import Syntax
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

-- parsing
--------------------------------------------------------------------------------

type Parser = Parsec Void String

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

withPos :: Parser Tm -> Parser Tm
withPos p = SrcPos <$> getSourcePos <*> p

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: String -> Parser ()
symbol s = lexeme (C.string s) $> ()

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

pArrow :: Parser ()
pArrow = symbol "→" <|> symbol "->"

keyword :: String -> Bool
keyword x = x == "let" || x == "in" || x == "λ" || x == "U"

pIdent :: Parser Name
pIdent = try $ do
  x <- takeWhile1P Nothing isAlphaNum
  guard (not (keyword x))
  x <$ ws

pKeyword :: String -> Parser ()
pKeyword kw = do
  _ <- C.string kw
  (takeWhile1P Nothing isAlphaNum *> empty) <|> ws

pAtom :: Parser Tm
pAtom =
  withPos ((Var <$> pIdent) <|> (U <$ lexeme (C.string "U")))
    <|> parens pTm

pBinder :: Parser String
pBinder = pIdent <|> C.string "_"

pSpine, pDataType, pPostulate, pLam, pPi, funOrSpine, pLet :: Parser Tm
pSpine = foldl1 App <$> some pAtom
pLam = do
  symbol "λ"
  xs <- some pBinder
  symbol "."
  t <- pTm
  pure (foldr Lam t xs)
pPi = do
  dom <- some (parens ((,) <$> some pBinder <*> (symbol ":" *> pTm)))
  pArrow
  cod <- pTm
  pure $ foldr (\(xs, a) t -> foldr (`Pi` a) t xs) cod dom
funOrSpine = do
  sp <- pSpine
  optional pArrow >>= \case
    Nothing -> pure sp
    Just _ -> Pi "_" sp <$> pTm
pDataType = do
  pKeyword "data"
  x <- pBinder
  cs <- many pCase
  symbol ";"
  foldl (\postulate (x', p) -> postulate . Postulate x' p) (Postulate x U) cs <$> pTm
  where
    pCase = do
      symbol "|"
      x <- pBinder
      symbol ":"
      p <- pTm
      return (x, p)
pPostulate = do
  pKeyword "postulate"
  x <- pBinder
  symbol ":"
  a <- pTm
  symbol ";"
  Postulate x a <$> pTm
pLet = do
  pKeyword "let"
  x <- pBinder
  symbol ":"
  a <- pTm
  symbol "="
  t <- pTm
  symbol ";"
  Let x a t <$> pTm

pTm :: Parser Tm
pTm = withPos $ choice [pLam, pDataType, pPostulate, pLet, try pPi, funOrSpine]

pSrc :: Parser Tm
pSrc = ws *> pTm <* eof
