module Lab2 where 

import Data.List
import System.Random
import Data.Char ( isLetter, isNumber )

type Conversion = [(Char,String)]

conversionTable = [('A', "10"), ('B', "11"), ('C', "12"), ('D', "13"), ('E',"14"),('F', "15"), ('G', "16"), ('H', "17"), ('I', "18"), ('J',"19"),('K',"20"), ('L', "21"),('M', "22"), ('N', "23"), ('O',"24"), ('P', "25"), ('Q', "26"), ('R', "27"), ('S', "28"),('T',"29"), ('U', "30"), ('V', "31"), ('W', "32"), ('X',"33"), ('Y', "34"), ('Z',"35")]

examples = ["AL47 2121 1009 0000 0002 3569 8741", "AD12 0001 2030 2003 5910 0100", "AT61 1904 3002 3457 3201", "AZ21 NABZ 0000 0000 1370 1000 1944", "BH67 BMAG 0000 1299 1234 56"]
negativeExamples = ["AL47$$!! 2122 1009 0000 0002 3569 8741", "ADEEEE12 0001 2030 2003 5910 0100", "ATXXXAA61 1904 3002 3457 3201", "AZ11 NABZ 0000 0000 1370 1000 1944", "BH67"]

--Question 1-- Time indication: 1 hour time spend (most time on testing, tried to automate test but stuck around IO output)
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

--Test question 1--
testSetQuestion1 = [[1,2,3],[3,3,3],[-1,-2,-3],[0,0,0],[2,2,0],[1,2,2],[2,2,8],[3,4,5]]
				   
testQuestion1 :: [[Integer]] -> IO ()
testQuestion1 [[]] = print ("done with all tests")
testQuestion1 (x:xs) = if True then 
						do print ("pass on: " ++ show x ++ " Result: " ++ show(triangle (x !! 0) (x !! 1) (x !! 2))) 	
						   testQuestion1 xs
					  else error ("failed test on" ++ show x) 

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

--Test question 4
testPositiveExamples :: [String] -> IO ()
testPositiveExamples [] = print ("done with all tests")
testPositiveExamples (x:xs) = if iban x then 
						do print ("pass on: " ++ show x) 	
						   testPositiveExamples xs
					  else error ("failed test on" ++ show x) 
					  
testNegativeExamples :: [String] -> IO ()
testNegativeExamples [] = print ("done with all tests")
testNegativeExamples (x:xs) = if iban x then 
						do print ("pass on: " ++ show x) 	
						   testNegativeExamples xs
					  else error ("failed test on" ++ show x) 				  

--Automated testing question 4
getRandomInteger :: Integer -> IO Integer 
getRandomInteger n = getStdRandom (randomR (0,n))

randomFlip :: Integer -> IO Integer 
randomFlip x = do 
		b <- getRandomInteger 1
		if b==0 then return x else return (-x)

genIntegerList :: IO [Integer]
genIntegerList = do 
  k <- getRandomInteger 20
  n <- getRandomInteger 3
  getIntegerL k n
 
getIntegerL :: Integer -> Integer -> IO [Integer]
getIntegerL _ 0 = return []
getIntegerL k n = do 
   x <-  getRandomInteger k
   y <- randomFlip x
   xs <- getIntegerL k (n-1)
   return (y:xs)
  
--Random char for creating iban  
getRandomChar :: Char -> IO Char
getRandomChar n = getStdRandom (randomR('a',n))

genCharList :: IO [Char]
genCharList = do
	k <- getRandomChar 'l'
	n <- getRandomInteger 10
	getCharL k n
	
getCharL :: Char -> Integer -> IO [Char]
getCharL _ 0 = return []
getCharL k n = do
	x <- getRandomChar k
	xs <- getCharL k (n-1)
	return (x:xs)	

--f = triangle needs to be replaced by the genIntegerList but problem with IO versus Integer... :/ how to fix?   
testR :: Integer -> Integer -> (Integer -> Integer -> Integer -> Shape) -> IO ()
testR k n f = if k /= n then  
						do print ("Result" ++ show(f 3 3 3)) 	
						   testR (k+1) n f
					  else print (show n ++ " tests passed") 
                  				  
testPost :: (Integer -> Integer -> Integer -> Shape) -> IO ()
testPost f = testR 1 100 f

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