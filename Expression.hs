module Expression where

import Types
import Data.List

data Expr =
    Val Integer
  | Var Name
  | Expr :+: Expr
  | Expr :-: Expr
  | Expr :*: Expr
  | Expr :/: Expr
  | Expr :%: Expr
  deriving(Eq, Ord)

instance Show Expr where
  show (Val a) = show a
  show (Var a) = show a
  show (a :+: b) = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (a :-: b) = "(" ++ show a ++ " - " ++ show b ++ ")"
  show (a :*: b) = "(" ++ show a ++ " * " ++ show b ++ ")"
  show (a :/: b) = "(" ++ show a ++ " / " ++ show b ++ ")"
  show (a :%: b) = "(" ++ show a ++ " % " ++ show b ++ ")"


vars :: Expr -> [Name]
vars = nub . sort . var
  where
  var (Val a) = []
  var (Var a) = [a]
  var (a :+: b) = var a ++ var b
  var (a :-: b) = var a ++ var b
  var (a :*: b) = var a ++ var b
  var (a :/: b) = var a ++ var b
  var (a :%: b) = var a ++ var b

