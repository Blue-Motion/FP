module Types(Name, Domain) where

type Name = String
type Domain = [Integer]


module Expression where

data Expr =
     Val Integer
   | Var Name
   | Expr :+: Expr
   | Expr :-: Expr
   | Expr :*: Expr
   | Expr :/: Expr

instance Show Expr where
show Val Integer = show Integer
show Val Name = show Name
show Expr :+: Expr = show Expr + " + " + Expr
show Expr :-: Expr = show Expr + " - " + Expr
show Expr :*: Expr = show Expr + " * " + Expr
show Expr :/: Expr = show Expr + " / " + Expr

