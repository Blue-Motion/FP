module Compare where

import Expression
import Data.List

data Relop = LessThan | LessEqual | Equal | GreaterEqual | Greater | NotEqual

data Comparison = Cmp Relop Expr Expr deriving Show

instance Show Relop where
	 show LessThan = "<"
	 show LessEqual =  "<="
	 show Equal = "="
	 show GreaterEqual = ">="
	 show Greater = ">"
	 show NotEqual = "#"

toComparison :: String -> Comparison
toComparison s = Cmp LessThan (toExpr a) (toExpr a)
	     where a = takeWhile (not (==(show Relop)) s)

toRelop :: String -> (Relop,String)
--torelop
	     