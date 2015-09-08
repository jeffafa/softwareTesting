module Lab2 where 

import Data.List
import System.Random
import Data.Char ( isLetter, isNumber )

type Conversion = [(Char,String)]

conversionTable = [('A', "10"), ('B', "11"), ('C', "12"), ('D', "13"), ('E',"14"),('F', "15"), ('G', "16"), ('H', "17"), ('I', "18"), ('J',"19"),('K',"20"), ('L', "21"),('M', "22"), ('N', "23"), ('O',"24"), ('P', "25"), ('Q', "26"), ('R', "27"), ('S', "28"),('T',"29"), ('U', "30"), ('V', "31"), ('W', "32"), ('X',"33"), ('Y', "34"), ('Z',"35")]

--Question 1--
data Shape = NoTriangle | Equilateral
             | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle  :: Integer -> Integer -> Integer -> Shape
triangle     a b c 	 | isTraingle a b c = NoTriangle
				 | isEquilateral a b c = Equilateral
				 | isIsosceles a b c = Isosceles
				 | isRectangular a b c = Rectangular
				 | otherwise = Other
				 
isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral a b c = if a == b && b == c then True else False	

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular a b c = if (a*a) + (b*b) == (c*c) then True else False

isIsosceles :: Integer -> Integer -> Integer -> Bool
isIsosceles a b c = if a == b || b == c then True else False

isTraingle :: Integer -> Integer -> Integer -> Bool
isTraingle a b c = if not (a + b > c && a + c > b && b + c > a) then True else False

--Question 2
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation x y =  if giveWhere (\a -> a == y) (perms x) == [] then False else True

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
   insrt x [] = [[x]]
   insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

--Question 3   
isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement x y =  if giveWhere (\a -> a == y) (deran x) == [] then False else True

deran :: Eq a => [a] -> [[a]]
deran [] = [[]]
deran x = remove (perms x) x

--Helping functions Question 2-3
giveWhere :: (a -> Bool) -> [a] -> [a]
giveWhere _ [] = []
giveWhere x (y:ys) | x y	= y : giveWhere x ys
				   | otherwise = giveWhere x ys 

remove :: Eq a => [[a]] -> [a] -> [[a]]
remove x [] = x
remove [[]] _ = [[]]
remove x y = giveWhere (\a -> a /= y) x     
   
--Question 4
iban :: String -> Bool
iban [] = False
iban xs = modDigits (digits (letterToNummeric (moveFirstFour (deleteAlpha xs))))

deleteAlpha :: String -> String
deleteAlpha y = filter (\x -> isNumber x || isLetter x) y

moveFirstFour :: [a] -> [a]
moveFirstFour x = drop 4 x ++ (take 4 x)

letterToNummeric :: String -> String
letterToNummeric [] = []
letterToNummeric (x:xs) = if (isLetter x) then (valueByKey x conversionTable) ++ letterToNummeric xs else x : letterToNummeric xs 

valueByKey :: Char -> Conversion -> String
valueByKey b (x:xs) = if (fst x) == b then snd x else valueByKey b xs

digits :: String -> Integer 
digits xs = read xs :: Integer

modDigits :: Integer -> Bool
modDigits x = x `mod` 97 == 1




