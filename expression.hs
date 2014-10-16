module Expression where

import Types

data Expr =
    Val Integer
  | Var Name
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr

instance Show Expr where
  show (Val a) = show a
  show (Var a) = show a
  show (a :+: b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (a :-: b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (a :*: b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (a :/: b) = "(" ++ show a ++ " / " ++ show b ++ ")"

vars :: Expr -> [Name]
vars (Val a) = []
vars (Var a) = [a]
vars (a :+: b) = (vars a)
--vars (a :-: b) = vars a ++ vars b
--vars (a :*: b) = vars a ++ vars b
--vars (a :/: b) = vars a ++ vars b