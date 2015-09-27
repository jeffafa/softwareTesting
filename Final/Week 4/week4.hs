module SetOrd (Set(..),emptySet,isEmpty,inSet,subSet,insertSet,
               deleteSet,powerSet,takeSet,(!!!),list2set,unionSet) where
 
import Data.List (nub,sort,delete) 
import System.Random
import Test.QuickCheck 

type Rel a = Set (a,a)

--Opdracht 1-- 3 hours
--halts/funny function 120-121
--page 130 Ai ?
--example 4.6 page 119 Paradoxes, types and type classes, I dont really understand the example.
-- if B is a subset of A (B C A), how is it that B is not a member of A?
--DeMorgan laws page 129

--Opdracht 2-- 5 hours
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
		
--QuickCheck part--
prop_commutativeAdd :: Integer -> Integer -> Bool
prop_commutativeAdd n m = n + m == m + n

--newtype Set a = Set [a] deriving (Eq,Ord)

instance (Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
                list <- arbitrary
                return $ Set list
				
positiveIntGen :: Gen (Positive Int)
positiveIntGen = arbitrary
    
--Opdracht 3-- 2.5 hour
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
							   
--Tests Opdracht 3 3 hours
testAssignement3 :: (Set Int -> Set Int -> Set Int) -> IO (Set Int)
testAssignement3 f   = do
	x <- genSet
	y <- genSet 
	return (f x y)						   
							   
--fx can be called by 1 100 union or intersection or difference
fx k n f = if k == n then print (show n ++ " tests passed")
                else 
				if True then
                    do (testAssignement3 f)
                       fx (k+1) n f
                    else error ("failed test on: ")

--QuickCheck
prop_union :: Set Int -> Set Int -> Bool
prop_union set1 set2 = isUnion set1 set2 (union set1 set2)

prop_intersection :: Set Int -> Set Int -> Bool
prop_intersection set1 set2 = isintersection set1 set2 (intersection set1 set2)

prop_difference :: Set Int -> Set Int -> Bool
prop_difference set1 set2 = isdifference set1 set2 (intersection set1 set2)

isUnion :: (Ord a) => Set a -> Set a -> Set a -> Bool
isUnion _ _ (Set[]) = True
isUnion set1 set2 (Set (x:xs)) = if (inSet x set1) || (inSet x set2) == True then (isUnion set1 set2 (Set xs)) else False 	   

isintersection :: (Ord a) => Set a -> Set a -> Set a -> Bool
isintersection _ _ (Set[]) = True
isintersection set1 set2 (Set (x:xs)) = if (inSet x set1) && (inSet x set2) == True then (isUnion set1 set2 (Set xs)) else False 	

isdifference :: (Ord a) => Set a -> Set a -> Set a -> Bool
isdifference _ _ (Set[]) = True
isdifference set1 set2 (Set (x:xs)) = if (not(inSet x set1)) || (not(inSet x set2)) == True then (isUnion set1 set2 (Set xs)) else False 

--Opdracht 4-- 2.5 hours
--Identity the pyramid  symbol		
--Equivalence classes, bit unclear...	
-- how where the the properties of relations related to each other like if it is refelexive it is also symmetric
--Classes and partitions

							   
--Opdracht 5 -- 1 hour

symClos :: Ord a => Rel a -> Rel a
symClos (Set[]) = (Set[])
symClos (Set(x:xs)) = if snd x /= fst x then union (Set [x,(snd x , fst x)]) (symClos (Set xs)) else union (Set [x]) (symClos (Set xs))  

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
		 	 
--Opdracht 7 3 hours

--Test set
testSet1 = Set [(1,2),(2,3),(3,4)]

--Length is 2 times the original
symTest :: Ord a => Rel a -> Bool
symTest xs = if lengthR (symClos xs) ==  2 * (lengthR xs) then True else False

lengthR :: Ord a => Rel a -> Int
lengthR (Set[]) = 0
lengthR (Set(x:xs)) = 1 + lengthR (Set xs)  

--Opdracht 8--
--Yes there is a difference between both let me explain why:

--If we have the Set R1 = (1,2) then the symmetric of R1 is R2 = (2,1),then the transitive closure of R1 and R2 is R3 = (1,1),(2,2) so R1 + R2 + R3 = (1,2,(2,1),(1,1),(2,2)
--In the other case: R1 = (1,2) then the transitive closure of R1 is R2 = (), then the symmetric closure of R1 and R2 is R3 = (2,1) so R1 + R2 + R3 = (1,2),(2,1)
   

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

addElem :: a-> [[a]] -> [[a]]
addElem x = map (x:)

--powerList :: [a] -> [[a]]
--powerList [] = [[]]
--powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

--data Boy = Matthew | Peter | Jack | Arnold | Carl 
--            deriving (Eq,Show)

data S = Void deriving (Eq,Show)
empty :: [S]
empty = []
