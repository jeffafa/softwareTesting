module SetOrd (Set(..),emptySet,isEmpty,inSet,subSet,insertSet,
               deleteSet,powerSet,takeSet,(!!!),list2set,unionSet) where
 
import Data.List (nub,sort,delete) 
import System.Random
import Test.QuickCheck 

type Rel a = Set (a,a)

--Opdracht 1-- 3 hours
--halts/funny function 120-121
--page 130 Ai ?

--Opdracht 2--
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 10
  n <- getRandomInt 10
  getIntL k n
 
getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   xs <- getIntL k (n-1)
   return (x:xs)

genSet :: IO (Set Int)
genSet = do
		x <- genIntList
		return (list2set x)
    
--Opdracht 3-- 1.5 hour
union :: (Ord a) => Set a -> Set a -> Set a
union (Set []) ys       = ys 
union (Set (x:xs)) ys = insertSet x $ union (Set xs) (deleteSet x ys) 

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set []) ys = (Set [])
intersection (Set (x:xs)) ys  | inSet x ys = insertSet x $ intersection (Set xs) ys
							   | otherwise = intersection (Set xs) ys
							   
difference :: (Ord a) => Set a -> Set a -> Set a
difference (Set []) ys = (Set [])
difference (Set (x:xs)) ys  | not (inSet x ys) = insertSet x $ difference (Set xs) ys
							   | otherwise = difference (Set xs) ys
							   
--Opdracht 4--

							   
							   
--Opdracht 5 -- 1 hour

symClos :: Ord a => Rel a -> Rel a
symClos (Set[]) = (Set[])
symClos (Set(x:xs)) = union (Set [x,(snd x , fst x)]) (symClos (Set xs)) 

--Opdracht 6-- 3 hours

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
(Set r) @@ (Set s) = 
  Set (nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ])
  
trClos :: Ord a => Rel a -> Rel a 
trClos xs | transR xs = xs
		  | otherwise = trClos (union xs (xs @@ xs))

transR :: Ord a => Rel a -> Bool
transR (Set []) = True
transR (Set s) = and [trans pair (Set s) | pair <- s] where 
		trans (x,y) (Set r) = 
		 and [ inSet (x,v) (Set r) | (u,v) <- r, u == y] 

{-- Sets implemented as ordered lists without duplicates --} 

newtype Set a = Set [a] deriving (Eq,Ord)

instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) str = showSet s str

showSet []     str = showString "{}" str
showSet (x:xs) str = showChar '{' ( shows x ( showl xs str))
     where showl []     str = showChar '}' str
           showl (x:xs) str = showChar ',' (shows x (showl xs str))

emptySet  :: Set a       
emptySet = Set []

isEmpty  :: Set a -> Bool            
isEmpty (Set []) = True
isEmpty _        = False

inSet  :: (Ord a) => a -> Set a -> Bool  
inSet x (Set s) = elem x (takeWhile (<= x) s)

subSet :: (Ord a) => Set a -> Set a -> Bool
subSet (Set []) _       = True  
subSet (Set (x:xs)) set = (inSet x set) && subSet (Set xs) set 

insertSet :: (Ord a) => a -> Set a -> Set a 
insertSet x (Set s) = Set (insertList x s) 

insertList x [] = [x]
insertList x ys@(y:ys') = case compare x y of 
                                 GT -> y : insertList x ys' 
                                 EQ -> ys 
                                 _  -> x : ys 

deleteSet :: Ord a => a -> Set a -> Set a 
deleteSet x (Set s) = Set (deleteList x s)

deleteList x [] = []
deleteList x ys@(y:ys') = case compare x y of 
                                 GT -> y : deleteList x ys'
                                 EQ -> ys'
                                 _  -> ys

list2set :: Ord a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)
-- list2set xs = Set (foldr insertList [] xs)

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) = 
   Set (sort (map (\xs -> (list2set xs)) (powerList xs)))

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs) 
                     ++ (map (x:) (powerList xs))

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs) 

infixl 9 !!!

(!!!) :: Eq a => Set a -> Int -> a 
(Set xs) !!! n = xs !! n

unionSet :: (Ord a) => Set a -> Set a -> Set a 
unionSet (Set [])     set2  =  set2
unionSet (Set (x:xs)) set2  = 
   insertSet x (unionSet (Set xs) set2)

