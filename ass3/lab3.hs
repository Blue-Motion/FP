import Data.List
import Expression
import Types

--ex1
brac :: String -> String
brac b = "(" ++ b ++ ")"

instance Show Expr where
	show (Var x) = x
	show (Val x) = show x
	show (e :+: e') = brac(show e ++ " + " ++ show e')
	show (e :-: e') = brac(show e ++ " - " ++ show e')
	show (e :*: e') = brac(show e ++ " * " ++ show e')
	show (e :/: e') = brac(show e ++ " / " ++ show e')
	show (e :%: e') = brac(show e ++ " % " ++ show e')
	
vars (Val a) = []
vars (Var a) = [a]
vars (a :+: b) = nub(vars a ++ vars b)
vars (a :-: b) = nub(vars a ++ vars b)
vars (a :*: b) = nub(vars a ++ vars b)
vars (a :/: b) = nub(vars a ++ vars b)
