module Main where
main = undefined

{- V1
 
data Expr
    = Lit Integer
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq, Show)

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)
-}

class Eval a where
    eval :: a -> Integer

data Lit = Lit Integer

instance Eval Lit where
    eval (Lit x) = x

data Add l r = Add l r

instance (Eval l, Eval r) => Eval (Add l r) where
    eval (Add l r) = eval l + eval r

data Mul l r = Mul l r

instance (Eval l, Eval r) => Eval (Mul l r) where
    eval (Mul l r) = eval l * eval r

data Sub l r = Sub l r

instance (Eval l, Eval r) => Eval (Sub l r) where
    eval (Sub l r) = eval l - eval r
