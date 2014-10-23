module Expression where

import Types
import Data.List
import Valuation

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
--cant the statements below be becombined?
--  var ((a -> b -> a)) = var a ++ var b

  var (a :+: b) = var a ++ var b
  var (a :-: b) = var a ++ var b
  var (a :*: b) = var a ++ var b
  var (a :/: b) = var a ++ var b
  var (a :%: b) = var a ++ var b

evalExpr :: Expr -> Valuation -> Integer
evalExpr e v
	 | ((length . vars) e) > (length v) = error "not enough valuated parameters"
	 | otherwise = read (show (eval e v))
	   where eval (Var x) v = head [snd n | n <- v, (fst n) == x] 
	   	 eval (Val x) v = x
		 eval (a :+: b) v = eval a v + eval b v
		 eval (a :-: b) v = eval a v - eval b v
		 eval (a :*: b) v = eval a v * eval b v
		 eval (a :/: b) v = div (eval a v) (eval b v)
		 eval (a :%: b) v = mod (eval a v) (eval b v)

--this cant be the right way, this takes >9000 patterns
simplifyExpr :: Expr -> Expr
simplifyExpr ((Val a) :+: (Val b)) = Val (a + b)
simplifyExpr ((Val a) :-: (Val b)) = Val (a - b)
simplifyExpr ((Val a) :*: (Val b)) = Val (a * b)
simplifyExpr ((Val a) :/: (Val b)) = Val (div a b)
simplifyExpr ((Val a) :%: (Val b)) = Val (mod a b)
simplifyExpr (a :+: b) = simplifyExpr (a :+: simplifyExpr b)
simplifyExpr (a :-: b) = simplifyExpr (a :-: simplifyExpr b)
simplifyExpr (a :*: b) = simplifyExpr (a :*: simplifyExpr b)
simplifyExpr (a :/: b) = simplifyExpr (a :/: simplifyExpr b)
simplifyExpr (a :%: b) = simplifyExpr (a :%: simplifyExpr b)
simplifyExpr a = a

