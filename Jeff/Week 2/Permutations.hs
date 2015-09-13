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
   
--Assignment 2 tests
--Comparing it to the permutation function in Data.List
testPerms :: (Eq a, Ord a) => [a]->Bool
testPerms [] = True
testPerms x = (sort(perms x)) == (sort(permutations x))

--Testing isPermutation (remember it tests the function its result doesn't mean it is a permutation)
testIsPerm :: Eq a => [a] -> [a] -> Bool
testIsPerm [] [] = True
testIsPerm x y = if (isPermutation x y) == (elem y (permutations x)) then True else False 

--Assignment 3   
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement x y =  if giveWhere (\a -> a == y) (deran x) == [] then False else True

deran :: Eq a => [a] -> [[a]]
deran [] = [[]]
deran x = remove (perms x) x

--Assignment 3 Testing
testDeran :: (Eq a, Ord a) => [a] -> Bool
testDeran [] = True
testDeran x = (sort(remove (permutations x) x)) == (sort(deran x))

testDeran2 :: Eq a => [a] -> Bool
testDeran2 [] = True
testDeran2 x = elem x (deran x)

--Remember this test the function. Doesn't tell if it is a derangement
testIsDerangement :: Eq a => [a] -> [a] -> Bool
testIsDerangement [] [] = True
testIsDerangement x y = if (isDerangement x y) == (elem y (derangement x)) then True else False

--Helping functions
giveWhere :: (a -> Bool) -> [a] -> [a]
giveWhere _ [] = []
giveWhere x (y:ys) | x y	= y : giveWhere x ys
				   | otherwise = giveWhere x ys 

remove :: Eq a => [[a]] -> [a] -> [[a]]
remove x [] = x
remove [[]] _ = [[]]
remove x y = giveWhere (\a -> a /= y) x  

--Different derangement function (found online to compare own Derangement function)
derangement xs = drg xs xs

drg [] ys = return []
drg xs (y:ys) =
  delete y xs >>=
  \x -> map (x:) (drg (delete x xs) ys)