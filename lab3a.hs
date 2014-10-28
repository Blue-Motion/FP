import Data.List
import Data.Char

import Compare
import Expression
import Types
import Valuation

evalExpr :: Expr -> Valuation -> Integer
evalExpr e v
	 | ((length . vars) e) > (length v) = error "not enough valuated parameters"
	 | otherwise = eval e v
	   where eval (Var x) v = head [snd n | n <- v, (fst n) == x] 
	   	 eval (Val x) v = x
		 eval (a :+: b) v = eval a v + eval b v
		 eval (a :-: b) v = eval a v - eval b v
		 eval (a :*: b) v = eval a v * eval b v
		 eval (a :/: b) v = div (eval a v) (eval b v)
		 eval (a :%: b) v = mod (eval a v) (eval b v)

evalCmp :: Comparison -> Valuation -> Bool
evalCmp (Cmp LessThan eOne eTwo) xs 	= (evalExpr eOne xs) < 	(evalExpr eTwo xs)
evalCmp (Cmp LessEqual eOne eTwo) xs 	= (evalExpr eOne xs) <= (evalExpr eTwo xs)
evalCmp (Cmp Equal eOne eTwo) xs 		= (evalExpr eOne xs) == (evalExpr eTwo xs)
evalCmp (Cmp Greater eOne eTwo) xs 		= (evalExpr eOne xs) > 	(evalExpr eTwo xs)
evalCmp (Cmp GreaterEqual eOne eTwo) xs = (evalExpr eOne xs) >= (evalExpr eTwo xs)
evalCmp (Cmp NotEqual eOne eTwo) xs 	= (evalExpr eOne xs) /= (evalExpr eTwo xs)