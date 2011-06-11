import Data.List

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

sub :: Expr -> Expr -> Expr
sub (Num a) (Num b)         = Num $ a - b
sub (Num n) (Array a)       = Array $ drop (fromInteger n) a
sub (Array a) (Num n)       = Array $ takeFromEnd n a
sub (Array a) (Array b)     = Array $ remove a b
sub (Num n) (Matrix m)      = Matrix $ drop (fromInteger n) m
sub (Matrix m) (Num n)      = Matrix $ takeFromEnd n m

takeFromEnd :: Integer -> [a] -> [a]
takeFromEnd n as = take (length as - fromInteger n) as

remove :: (Eq a) => [a] -> [a] -> [a]
remove [] bs = []
remove aas@(a:as) bs
    | length aas < l  = []
    | take l aas == bs = remove (drop l aas) bs
    | otherwise       = a : remove as bs
    where l = length bs

mul :: Expr -> Expr -> Expr
mul (Num a) (Num b)         = Num $ a * b
mul (Array a) (Num n)       = Array $ extendRight n a
mul t@(Num _) a@(Array _)   = mul a t
mul (Array a) (Array b)     = Matrix [a,b]
mul (Matrix m) (Num n)      = Matrix $ map (extendRight n) m
mul (Array a) (Matrix m)    = Matrix $ zipWith (:) a m
mul (Matrix m) (Array a)    = Matrix $ zipWith (\x y -> x++[y]) m a
mul (Matrix a) (Matrix b)   = Matrix $ zipWith (++) a b

extendRight :: Integer -> [a] -> [a]
extendRight n as = concat $ take (fromInteger n) $ repeat as
