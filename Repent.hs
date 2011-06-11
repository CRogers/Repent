import Data.List
import Char
import Stack

data Expr = Num { num :: Integer } | Var { varStr :: String } | Array [Integer] | Matrix [[Integer]]
            deriving (Show, Eq)

type EStack = Stack Expr

eval :: EStack -> Char -> EStack
eval s c
    | c == ' ' = s
    | isDigit c = let d = charToDigit c in          -- If the previous number is on the stack, concat with new Num
                    if (not $ isEmpty s) && (isNum $ peek s) then let (i,t) = pop s in push t $ Num $ (num i)*10 + d
                                      else push s $ Num d
    | otherwise = pop2into s $ case c of
                    '+' -> addE
                    '-' -> subE
                    '*' -> mulE
                    '/' -> divE


run :: EStack -> String -> EStack
run = foldl eval


isNum :: Expr -> Bool
isNum (Num _) = True
isNum _       = False

charToDigit :: Char -> Integer
charToDigit c = read [c]


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
