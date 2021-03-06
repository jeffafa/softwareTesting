module Workshop6Answers where

import Data.List
import Data.Char

data Blt a = Leaf a | Node (Blt a) (Blt a) deriving (Eq,Show)

exampleTree :: Blt String
exampleTree = Node (Node (Leaf "Hoare, Tony")
                         (Leaf "Turing, Alan"))
                   (Leaf "Goedel, Kurt")

leafCount ::  Blt a -> Int
leafCount (Leaf _) = 1
leafCount (Node left right) = leafCount left + leafCount right + 1

mapB :: (a -> b) -> Blt a -> Blt b
mapB f (Leaf x) = (Leaf (f x))
mapB f (Node t1 t2) = Node (mapB f t1) (mapB f t2)

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

example1 = T 1 [T 2 [], T 3 []]
example2 = T 0 [example1,example1,example1]

count :: Tree a -> Int 
count (T _ ts) = 1 + sum (map count ts)

depth :: Tree a -> Int 
depth (T _ []) = 0
depth (T _ ts) = foldl max 0 (map depth ts) + 1

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (T x ts) = T (f x) (map (mapT f) ts)

collect :: Tree a -> [a]
collect (T x ts) = x : concat (map collect ts)

foldT :: (a -> [b] -> b) -> Tree a -> b
foldT f (T x ts) = f x (map (foldT f) ts)

count' :: Tree a -> Int
count' = foldT (\ _ ns -> sum ns + 1)

depth' :: Tree a -> Int
depth' = foldT (\ _ ds -> if null ds then 0 else maximum ds + 1)

collect' :: Tree a -> [a]
collect' = foldT (\ x lists ->  x : concat lists)

mapT' :: (a -> b) -> Tree a -> Tree b
mapT' f = foldT (\ x ts ->  T (f x) ts)

grow :: (node -> [node]) -> node -> Tree node 
grow step seed = T seed (map (grow step) (step seed))

tree n = grow (f n) (1,1)

f n = \ (x,y) -> if x+y <= n then [(x,x+y),(x+y,y)] else []

search :: (node -> [node]) 
       -> (node -> Bool) -> [node] -> [node]
search children goal [] = []
search children goal (x:xs) 
  | goal x    = x : search children goal xs
  | otherwise = search children goal ((children x) ++ xs)

