--Permutations
import Data.List
import System.Random

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation x y =  if giveWhere (\a -> a == y) (perms x) == [] then False else True

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
   insrt x [] = [[x]]
   insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)
   
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement x y =  if giveWhere (\a -> a == y) (deran x) == [] then False else True

deran :: Eq a => [a] -> [[a]]
deran [] = [[]]
deran x = removeDuplicates (perms x) x

giveWhere :: (a -> Bool) -> [a] -> [a]
giveWhere _ [] = []
giveWhere x (y:ys) | x y	= y : giveWhere x ys
				   | otherwise = giveWhere x ys 

removeDuplicates :: Eq a => [[a]] -> [a] -> [[a]]
removeDuplicates x [] = x
removeDuplicates [[]] _ = [[]]
removeDuplicates x y = giveWhere (\a -> a /= y) x  