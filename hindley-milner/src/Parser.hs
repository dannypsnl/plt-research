module Parser (
  expr
) where
import Lang (Expr(..), Op(..))

import Data.Functor.Identity (Identity)
import Text.Parsec (between, many, parse, try, (<|>))
import Text.Parsec.Char (digit, letter, noneOf, oneOf)
import Text.Parsec.Expr (Assoc(AssocLeft), Operator(Infix), buildExpressionParser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Prim (ParsecT, unexpected)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token

table = [
  [binary "*" (Binary Mul) AssocLeft, binary "/" (Binary Div) AssocLeft]
  , [binary "+" (Binary Add) AssocLeft, binary "-" (Binary Sub) AssocLeft]
  , [binary "==" (Binary Equal) AssocLeft, binary "/=" (Binary NotEqual) AssocLeft]
  ]

binary name fun assoc = Infix (do { reservedOp name; return fun }) assoc

expr :: Parser Expr
expr = buildExpressionParser table term

term :: Parser Expr
term =
  parens expr
  <|> int
  <|> string
  <|> func
  <|> application
  <|> letBinding
  <|> var

func :: Parser Expr
func = do
  reserved "fn"
  param <- identifier
  reservedOp "="
  Func param <$> expr

application :: Parser Expr
application = do
  reserved "call"
  f <- expr
  Application f <$> expr

letBinding :: Parser Expr
letBinding = do
  reserved "let"
  newVar <- identifier
  reservedOp "="
  e <- expr
  reserved "in"
  Let newVar e <$> expr

var :: Parser Expr
var = do
  v <- identifier
  return $ case v of
    "true" -> Bool True
    "false" -> Bool False
    _ -> Var v
string :: Parser Expr
string = do
  reservedOp "\""
  content <- many $ noneOf "\""
  reservedOp "\""
  return $ String content
int :: Parser Expr
int = Int <$> integer

reserved :: String -> ParsecT String () Identity ()
reserved = Token.reserved lexer
identifier :: ParsecT String () Identity String
identifier = Token.identifier lexer
integer :: ParsecT String () Identity Integer
integer = Token.integer lexer
reservedOp :: String -> ParsecT String () Identity ()
reservedOp = Token.reservedOp lexer
parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = Token.parens lexer
brackets :: ParsecT String () Identity a -> ParsecT String () Identity a
brackets = Token.brackets lexer

lexer :: Token.GenTokenParser String () Identity
lexer = Token.makeTokenParser languageDef

symbol :: Parser Char
symbol = oneOf "_"
languageDef :: Token.GenLanguageDef String () Identity
languageDef = emptyDef {
  Token.commentStart = "#|"
  , Token.commentEnd = "|#"
  , Token.commentLine = ";"
  , Token.identStart = letter <|> symbol
  , Token.identLetter = digit <|> letter <|> symbol
  , Token.reservedOpNames = [
    "+"
    , "-"
    , "*"
    , "/"
    , "=="
    , "/="
    ]
  }
