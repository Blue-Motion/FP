module Expression where

import Types
import Data.Char
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

instance Num Expr where
	a + b = a :+: b
	a - b = a :-: b
	a * b = a :*: b
	

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
	 | otherwise = eval e v
	   where eval (Var x) v = head [snd n | n <- v, (fst n) == x] 
	   	 eval (Val x) v = x
		 eval (a :+: b) v = eval a v + eval b v
		 eval (a :-: b) v = eval a v - eval b v
		 eval (a :*: b) v = eval a v * eval b v
		 eval (a :/: b) v = div (eval a v) (eval b v)
		 eval (a :%: b) v = mod (eval a v) (eval b v)

lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
	| elem c"+-*/%" = [c]:(lexer cs)
	| elem c " " = lexer cs
	| isAlpha c = (c:takeWhile isAlpha cs): lexer(dropWhile isAlpha cs)
	| isDigit c = (c:takeWhile isDigit cs): lexer(dropWhile isDigit cs)
	| otherwise = error "Syntax Error: invalid character in input"		 
		 
parser :: String -> (Expr,[String])
parser str = parseE (lexer str)	

parseE :: [String] -> (Expr,[String])
parseE tokens = parseE' acc rest
	where (acc, rest) = parseT tokens	 
		 	 
parseT :: [String] -> (Expr,[String])
parseT tokens = parseT' acc rest
	where (acc, rest) = parseF tokens	 
		 	 
parseE' :: Expr -> [String] -> (Expr,[String])
parseE' accepted ("+":tokens) = parseE' (accepted :+: term) rest
	where (term, rest) = parseT tokens
parseE' accepted ("-":tokens) = parseE' (accepted :-: term) rest
	where (term, rest) = parseT tokens
parseE' accepted tokens = (accepted, tokens)

parseT' :: Expr -> [String] -> (Expr,[String])
parseT' accepted ("*":tokens) = parseT' (accepted :*: term) rest
	where (term, rest) = parseF tokens
parseT' accepted ("/":tokens) = parseT' (accepted :/: term) rest
    where (term, rest) = parseF tokens
parseT' accepted ("%":tokens) = parseT' (accepted :%: term) rest
    where (term, rest) = parseF tokens
parseT' accepted tokens =  (accepted, tokens)

parseF :: [String] -> (Expr, [String])
parseF [] = error "Parse error...abort"
parseF (tok:tokens)
	| tok == "("  		 = (expr, tail rest)
	| isAlpha	(head tok) = (Var tok, tokens)
	| isDigit	(head tok) = (Val (read tok), tokens)
	| otherwise = error ("Syntax Error: " ++ tok)
	where
		(expr, rest) = parseE tokens

toExpr :: String -> Expr
toExpr str = fst (parser str)
-- 		 
simplifyExpr :: Expr -> Expr
simplifyExpr ((Val a) :+: (Val b)) = Val (a + b)
simplifyExpr ((Val a) :-: (Val b)) = Val (a - b)
simplifyExpr ((Val a) :*: (Val b)) = Val (a * b)
simplifyExpr ((Val a) :/: (Val b)) = Val (div a b)
simplifyExpr ((Val a) :%: (Val b)) = Val (mod a b)
simplifyExpr (a :+: b) = (a + simplifyExpr b)
simplifyExpr (a :-: b) = (a - simplifyExpr b)
simplifyExpr (a :*: b) = (a * simplifyExpr b)
--simplifyExpr (a :/: b) = simplifyExpr (simplifyExpr a / simplifyExpr b)
--simplifyExpr (a :%: b) = simplifyExpr (simplifyExpr a % simplifyExpr b)
simplifyExpr a = a

