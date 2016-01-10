module SetEq (Set(..), emptySet, isEmpty, inSet, subSet, insertSet,
                deleteSet, powerSet, takeSet, list2set,(!!!))

where

import Data.List (delete)

infixl 9 !!!

newtype Set a = Set [a] deriving (Eq,Ord)

type Rel a = Set (a,a)
    
instance (Show a) => Show (Set a) where
    showsPrec _ (Set s) str = showSet s str
    
showSet []      str = showString "{}" str
showSet (x:xs)  str = showChar '{' (shows x (showl xs str))
    where showl []     str = showChar '}' str
          showl (x:xs) str = showChar ',' (shows x (showl xs str))
          
emptySet :: Set a
emptySet = Set []

isEmpty :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _        = False

inSet :: (Eq a) => a -> Set a -> Bool
inSet x (Set s) = elem x s

mapSet :: (Eq a) => (a -> b) -> Set a -> Set b
mapSet f (Set xs) = Set $ map f xs

filterSet :: (Eq a) => (a -> Bool) -> Set a -> Set a
filterSet f (Set xs) = Set $ filter f xs

subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x:xs)) set = (inSet x set) && subSet (Set xs) set

insertSet :: (Eq a) => a -> Set a -> Set a
insertSet x (Set ys) | inSet x (Set ys) = Set ys
                     | otherwise        = Set (x:ys)
                     
deleteSet :: Eq a => a -> Set a -> Set a
deleteSet x (Set xs) = Set (delete x xs)

list2set :: Eq a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)

powerSet :: Eq a => Set a -> Set (Set a)
powerSet (Set xs) = Set (map (\xs -> (Set xs)) (powerList xs))

powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

takeSet :: Eq a => Int -> Set a -> Set a
takeSet n (Set xs) = Set (take n xs)

(!!!) :: Eq a => Set a -> Int -> a
(Set xs) !!! n = xs !! n

unionSet :: Eq a => Set a -> Set a -> Set a
unionSet (Set []) (Set []) = Set []
unionSet (Set []) set2 = set2
unionSet set1 (Set []) = set1
unionSet (Set (x:xs)) set2 = unionSet (Set xs) $ insertSet x set2                           

intersectSet :: Eq a => Set a -> Set a -> Set a
intersectSet (Set []) _ = Set []
intersectSet _ (Set []) = Set []
intersectSet set1 set2 = filterSet (`inSet` set1) set2  

differenceSet :: Eq a => Set a -> Set a -> Set a
differenceSet (Set []) set2 = set2
differenceSet set1 (Set []) = set1
differenceSet set1 (Set (y:ys)) | y `inSet` set1 = differenceSet (deleteSet y set1) (Set ys) 
                                | otherwise = differenceSet set1 (Set ys)

idR :: Ord a => Set a -> Rel a
idR (Set xs) = Set [(x,x) | x <- xs]
                                
simpleSet1 = Set [1,2,3]
simpleSet2 = Set [1,2,4]
simpleSet3 = Set [5,6,7]

