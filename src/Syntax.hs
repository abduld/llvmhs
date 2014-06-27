module Syntax where

  type Name = String


  data Expr
    = Float Double
    | BinOp Name Expr Expr
    | Var String
    | Call Name [Expr]
    | Function Name [Expr] Expr
    | Extern Name [Expr]
    deriving (Eq, Ord, Show)
  
