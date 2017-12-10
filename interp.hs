-- This is a Lambda-calculus interpreter to reduce a Lambda-term to its normal form

data Expr = Var Char
    | Lam Char Expr
    | App Expr Expr

-- "Pretty" printing
instance Show Expr where
    show (Var v)    = [v]
    show (Lam x m) = "(\\" ++ [x] ++ "." ++ (show m) ++ ")"
    show (App m n) = "(" ++ (show m) ++ " " ++ (show n) ++ ")"

subst :: Expr -> Expr -> Expr -> Expr
subst (Var x) l (Var y) = if x == y then l else (Var x)
subst (Lam x m) l (Var y) = if x == y then (Lam x m) else (Lam x (subst m l (Var y)))
subst (App m n) l (Var y) = App (subst m l (Var y)) (subst n l (Var y))

-- Application case for beta reduction
appRedex :: Expr -> Expr -> Expr
appRedex (Lam x m) n = normalize (subst m n (Var x))
appRedex m n = App (normalize m) (normalize n)

-- Normalize the term
normalize :: Expr -> Expr
normalize (Var x) = Var x
normalize (Lam x m) = Lam x (normalize m)
normalize (App m n) = appRedex m n

-- Examples
ex1 = Lam 'x' (App (Var 'a') (Var 'x'))
ex2 = Lam 'y' (App (Var 'b') (Var 'y'))
ex3 = App ex2 (Var 'c')
ex4 = App ex1 ex3
