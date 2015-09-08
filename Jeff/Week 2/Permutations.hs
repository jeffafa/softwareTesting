--Permutations
import Data.List
import System.Random




--Assignment 2
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation x y =  if giveWhere (\a -> a == y) (perms x) == [] then False else True

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
   insrt x [] = [[x]]
   insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

--Assignment 3   
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement x y =  if giveWhere (\a -> a == y) (deran x) == [] then False else True

deran :: Eq a => [a] -> [[a]]
deran [] = [[]]
deran x = remove (perms x) x

--Helping functions
giveWhere :: (a -> Bool) -> [a] -> [a]
giveWhere _ [] = []
giveWhere x (y:ys) | x y	= y : giveWhere x ys
				   | otherwise = giveWhere x ys 

remove :: Eq a => [[a]] -> [a] -> [[a]]
remove x [] = x
remove [[]] _ = [[]]
remove x y = giveWhere (\a -> a /= y) x  