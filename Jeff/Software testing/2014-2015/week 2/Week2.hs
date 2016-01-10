module Week2

where 

import Data.List
import Data.Char
import System.Random

infix 1 ==> 

(==>) :: Bool -> Bool -> Bool
p ==> q = (not p) || q

forall = flip all

stronger, weaker :: [a] -> 
          (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x ==> q x)
weaker   xs p q = stronger xs q p 

neg :: (a -> Bool) -> a -> Bool
neg p = \ x -> not (p x)

infixl 2 .&&. 
infixl 2 .||.

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .&&. q = \ x -> p x && q x 

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .||. q = \ x -> p x || q x 

infixl 2 #

(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

infixl 1 $$

($$) :: a -> (a -> b) -> b
($$) = flip ($)

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z 

type Env = String -> Int

data Expr = I Int | V String 
          | Add Expr Expr 
          | Subtr Expr Expr 
          | Mult Expr Expr 
          deriving (Eq,Show)

eval :: Expr -> Env -> Int 
eval (I i) c = i 
eval (V name) c = c name
eval (Add e1 e2) c = (eval e1 c) + (eval e2 c)
eval (Subtr e1 e2) c = (eval e1 c) - (eval e2 c)
eval (Mult e1 e2) c = (eval e1 c) * (eval e2 c)

assign :: String -> Expr -> Env -> Env 
assign var expr c = let 
  value = eval expr c
 in 
  update c (var,value)

initc :: Env 
initc = \ _ -> undefined

example = initc $$ 
          assign "x" (I 3) # 
          assign "x" (Mult (V "x") (V "x")) #
          eval (V "x")

while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)

euclid m n = (m,n) $$
   while (\ (x,y) -> x /= y) 
         (\ (x,y) -> if x > y then (x-y,y) 
                              else (x,y-x)) #
         fst

whiler :: (a -> Bool) 
       -> (a -> a) -> (a -> b) -> a -> b
whiler p f r = r . while p f

euclid2 m n = (m,n) $$
           whiler (\ (x,y) -> x /= y) 
                  (\ (x,y) -> if x > y then (x-y,y) 
                                       else (x,y-x))
                  fst

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int
randomFlip x = do 
  b <- getRandomInt 1
  if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 10
  getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
    x <-  getRandomInt k
    y <- randomFlip x
    xs <- getIntL k (n-1)
    return (y:xs)

data Coin = C Int

w :: Coin -> Float 
w (C n) = if n == lighter then 1 - 0.01
          else if n == heavier then 1 + 0.01
          else 1

weight :: [Coin] -> Float
weight = sum . (map w)

balance :: [Coin] -> [Coin] -> Ordering 
balance xs ys = 
  if weight xs < weight ys then LT
  else if weight xs > weight ys then GT
  else EQ

lighter, heavier :: Int
lighter = 3
heavier = 0

