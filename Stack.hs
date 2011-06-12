module Stack (Stack(EmptyStack, Stack), isEmpty, push, pop, popN, pop2into, peek, dropWhileS) where

data Stack a = EmptyStack | Stack a (Stack a)
            deriving (Show, Eq)

isEmpty :: Stack a -> Bool
isEmpty EmptyStack = True
isEmpty _          = False

push :: Stack a -> a -> Stack a
push s i = Stack i s

pop :: Stack a -> (a, Stack a)
pop EmptyStack = error("Cannot pop from the empty stack")
pop (Stack i s) = (i,s)

popN :: Stack a -> Int -> ([a], Stack a)
popN s n = inner s n []
    where inner s 0 as = (as, s)
          inner s n as = let (i,t) = pop s in inner t (n-1) (as++[i])

pop2into :: Stack a -> (a -> a -> a) -> Stack a
pop2into s f = push u $ f i j
    where (i,t) = pop s
          (j,u) = pop t

peek :: Stack a -> a
peek EmptyStack = error("Cannot peek from an empty stack")
peek (Stack i _) = i

dropWhileS :: (a -> Bool) -> Stack a -> Stack a
dropWhileS _ EmptyStack         = EmptyStack
dropWhileS f st@(Stack i s)     = if f i then dropWhileS f s else st
