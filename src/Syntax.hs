module Syntax where

type Name = String


data Expr
  = Float Double
  | BinaryOp BinOp Expr Expr
  | Var String
  | Call Name [Expr]
  | Function Name [Expr] Expr
  | Extern Name [Expr]
  deriving (Eq, Ord, Show)


data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
