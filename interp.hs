-- This is a Lambda-calculus interpreter to reduce a Lambda-term to its normal form

data Expr = Var Char
    | Lam Char Expr
    | App Expr Expr
    deriving (Show)

subst :: Expr -> Expr -> Expr -> Expr
subst (Var x) l (Var y) = if x == y then l else (Var x)
subst (Lam x m) l (Var y) = if x == y then (Lam x m) else (Lam x (subst m l (Var y)))
subst (App m n) l (Var y) = App (subst m l (Var y)) (subst n l (Var y))
