data Expr = Num Integer | Var String | Array [Integer] | Matrix [[Integer]]
            deriving (Show, Eq)

add :: Expr -> Expr -> Expr
add (Num a) (Num b)         = Num $ a + b
add (Num n) (Array a)       = Array $ n : a
add (Array a) (Num n)       = Array $ a ++ [n]
add (Array a) (Array b)     = Array $ a ++ b
add (Array a) (Matrix m)    = Matrix $ a : m
add (Matrix m) (Array a)    = Matrix $ m ++ [a]
add (Matrix a) (Matrix b)   = Matrix $ a ++ b

mul :: Expr -> Expr -> Expr
mul (Num a) (Num b)         = Num $ a * b
mul (Array a) (Num t)       = Array $ concat $ take (fromInteger t) $ repeat a
mul t@(Num _) a@(Array _)   = mul a t
mul (Matrix a) (Matrix b)   = Matrix $ zipWith (zipWith (*)) a b
