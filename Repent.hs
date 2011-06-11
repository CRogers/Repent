import Data.List
import Char

data Expr = Num Integer | Var String | Array [Integer] | Matrix [[Integer]]
            deriving (Show, Eq)

data Stack a = EmptyStack | Stack a (Stack a)
            deriving (Show, Eq)
type EStack = Stack Expr


push :: Stack a -> a -> Stack a
push s i = Stack i s

pop :: Stack a -> (a, Stack a)
pop EmptyStack = error("Cannot pop from the emptystack")
pop (Stack i s) = (i,s)

popN :: Stack a -> Int -> ([a], Stack a)
popN s n = inner s n []
    where inner s 0 as = (as, s)
          inner s n as = let (i,t) = pop s in inner t (n-1) (as++[i])

pop2into :: Stack a -> (a -> a -> a) -> Stack a
pop2into s f = push u $ f i j
    where (i,t) = pop s
          (j,u) = pop t

eval :: EStack -> Char -> EStack
eval s c
    | isDigit c = push s $ Num $ read [c]
    | otherwise = pop2into s $ case c of
                    '+' -> addE
                    '-' -> subE
                    '*' -> mulE
                    '/' -> divE


run :: EStack -> String -> EStack
run = foldl eval



addE :: Expr -> Expr -> Expr
addE (Num a) (Num b)         = Num $ a + b
addE (Num n) (Array a)       = Array $ n : a
addE (Array a) (Num n)       = Array $ a ++ [n]
addE (Array a) (Array b)     = Array $ a ++ b
addE (Array a) (Matrix m)    = Matrix $ a : m
addE (Matrix m) (Array a)    = Matrix $ m ++ [a]
addE (Matrix a) (Matrix b)   = Matrix $ a ++ b

subE :: Expr -> Expr -> Expr
subE (Num a) (Num b)         = Num $ a - b
subE (Num n) (Array a)       = Array $ drop (fromInteger n) a
subE (Array a) (Num n)       = Array $ dropFromEnd n a
subE (Array a) (Array b)     = Array $ remove a b
subE (Num n) (Matrix m)      = Matrix $ drop (fromInteger n) m
subE (Matrix m) (Num n)      = Matrix $ dropFromEnd n m

dropFromEnd :: Integer -> [a] -> [a]
dropFromEnd n as = take (length as - fromInteger n) as

takeFromEnd :: Integer -> [a] -> [a]
takeFromEnd n as = drop (length as - fromInteger n) as

remove :: (Eq a) => [a] -> [a] -> [a]
remove [] bs = []
remove aas@(a:as) bs
    | length aas < l  = []
    | take l aas == bs = remove (drop l aas) bs
    | otherwise       = a : remove as bs
    where l = length bs

mulE :: Expr -> Expr -> Expr
mulE (Num a) (Num b)         = Num $ a * b
mulE (Array a) (Num n)       = Array $ extendRight n a
mulE t@(Num _) a@(Array _)   = mulE a t
mulE (Array a) (Array b)     = Matrix [a,b]
mulE (Matrix m) (Num n)      = Matrix $ map (extendRight n) m
mulE (Array a) (Matrix m)    = Matrix $ zipWith (:) a m
mulE (Matrix m) (Array a)    = Matrix $ zipWith (\x y -> x++[y]) m a
mulE (Matrix a) (Matrix b)   = Matrix $ zipWith (++) a b

divE :: Expr -> Expr -> Expr
divE (Num a) (Num b)         = Num $ a `div` b
divE (Array a) (Num n)       = Array $ takeFromEnd n a
divE (Num n) (Array a)       = Array $ take (fromInteger n) a
divE (Matrix m) (Num n)      = Matrix $ takeFromEnd n m
divE (Num n) (Matrix m)      = Matrix $ take (fromInteger n) m

extendRight :: Integer -> [a] -> [a]
extendRight n as = concat $ take (fromInteger n) $ repeat as
