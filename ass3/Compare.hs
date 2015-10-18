module Compare where
import Expression
import Valuation

data Relop = LessThan | LessEqual | Equal | GreaterEqual | Greater | NotEqual deriving (Show)
data Comparison = Cmp Relop Expr Expr deriving (Show)

comparisonOperator :: [Char] -> Relop
comparisonOperator "<" = LessThan
comparisonOperator "<=" = LessEqual
comparisonOperator "=" = Equal
comparisonOperator ">" = Greater
comparisonOperator ">=" = GreaterEqual
comparisonOperator "#" = NotEqual

toComparison :: String -> Comparison
toComparison str = Cmp (comparisonOperator (getComparator str)) (toExpr(expression str)) (toExpr((reverse . expression . reverse ) str))

expression :: [Char] -> [Char]
expression str = takeWhile (not.(\x -> elem x "<>=#")) str

getComparator:: [Char] -> [Char]
getComparator str = (reverse (dropWhile (not.(\x -> elem x "<>=#")) (reverse (dropWhile (not.(\x -> elem x "<>=#")) str))))

evalCmp :: Comparison -> Valuation -> Bool
evalCmp (Cmp LessThan expression1 expression2) xs = (evalExpr expression1 xs) < (evalExpr expression2 xs)
evalCmp (Cmp LessEqual expression1 expression2) xs = (evalExpr expression1 xs) <= (evalExpr expression2 xs)
evalCmp (Cmp Equal expression1 expression2) xs 	= (evalExpr expression1 xs) == (evalExpr expression2 xs)
evalCmp (Cmp Greater expression1 expression2) xs = (evalExpr expression1 xs) > 	(evalExpr expression2 xs)
evalCmp (Cmp GreaterEqual expression1 expression2) xs= (evalExpr expression1 xs) >= (evalExpr expression2 xs)
evalCmp (Cmp NotEqual expression1 expression2) xs = (evalExpr expression1 xs) /= (evalExpr expression2 xs)