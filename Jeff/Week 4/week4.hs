import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import System.IO.Unsafe

type Rel a = Set (a,a)

--assignment 1 -- 2 hours
--example 4.6 page 119 Paradoxes, types and type classes, I dont really understand the example.
-- if B is a subset of A (B C A), how is it that B is not a member of A?
--DeMorgan laws page 129

--assignment 2 -- 6 hours
randInRange :: Int -> Int -> IO Int
randInRange a b = getStdRandom $ randomR (a, b)

randomInt :: Int -> Int -> Int
randomInt x y = unsafePerformIO (randInRange x y) 

randomInts :: Int -> [Int]
randomInts 0 = []
randomInts x = [(randomInt 0 100)] ++ (randomInts (x-1))

randomSet :: Set Int
randomSet = list2set(randomInts (randomInt 0 10))

--assignment 3 -- 4 hours
union' :: (Ord a) => Set a -> Set a -> Set a
union' (Set []) ys       = ys 
union' (Set (x:xs)) ys = insertSet x $ union' (Set xs) (deleteSet x ys) 

intersection' :: (Ord a) => Set a -> Set a -> Set a
intersection' (Set []) ys = (Set [])
intersection' (Set (x:xs)) ys  | inSet x ys = insertSet x $ intersection' (Set xs) ys
							   | otherwise = intersection' (Set xs) ys
							   
difference' :: (Ord a) => Set a -> Set a -> Set a
difference' (Set []) ys = (Set [])
difference' (Set (x:xs)) ys  | not (inSet x ys) = insertSet x $ difference' (Set xs) ys
							   | otherwise = difference' (Set xs) ys
--assignment 4 -- 2hours
-- how where the the properties of relations related to each other like if it is refelexive it is also symmetric
--Classes and partitions
-- pyramid  symbol

--assignment 5 -- 2 hours
symClos :: Ord a => Rel a -> Int
symClos (Set[]) = 0--(Set[])
symclos (Set (x:xs)) | snd x /= fst x = union (set [x, snd x,fst x)]) (symClos (Set xs))
					 | otherwise = union (Set [x]) (symClos (Set xs))

--assignment 6

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
(Set r) @@ (Set s) = 
  Set (nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ])
  
trClos :: Ord a => Rel a -> Rel a 
trClos xs | transR xs = xs
		  | otherwise = trClos (union' xs (xs @@ xs))

transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s) = and [trans pair (Set s) | pair <- s] where 
		trans (x,y) (Set r) = 
		 and [ inSet (x,v) (Set r) | (u,v) <- r, u == y] 
		 
--assignment 7

--assignment 8

addElem :: a-> [[a]] -> [[a]]
addElem x = map (x:)

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

--data Boy = Matthew | Peter | Jack | Arnold | Carl 
--            deriving (Eq,Show)

data S = Void deriving (Eq,Show)
empty :: [S]
empty = []