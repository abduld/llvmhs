module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative ((<$>))

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

binary s f assoc = Ex.Infix (reservedOp s >> return (BinaryOp f)) assoc

binops = [ [ binary "*" Times Ex.AssocLeft
           , binary "/" Divide Ex.AssocLeft ]
         , [ binary "+" Plus Ex.AssocLeft
           , binary "-" Minus Ex.AssocLeft ]
         ]

expr :: Parser Expr
expr =  Ex.buildExpressionParser binops factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor = try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> (parens expr)

defn :: Parser Expr
defn = try extern
    <|> try function
    <|> expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

toplevel :: Parser [Expr]
toplevel = many $ do
    def <- defn
    reservedOp ";"
    return def

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
