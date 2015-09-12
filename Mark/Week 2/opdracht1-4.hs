module Lab2 where 

import Data.List
import System.Random
import Data.Char ( isLetter, isNumber )

type Conversion = [(Char,String)]

conversionTable = [('A', "10"), ('B', "11"), ('C', "12"), ('D', "13"), ('E',"14"),('F', "15"), ('G', "16"), ('H', "17"), ('I', "18"), ('J',"19"),('K',"20"), ('L', "21"),('M', "22"), ('N', "23"), ('O',"24"), ('P', "25"), ('Q', "26"), ('R', "27"), ('S', "28"),('T',"29"), ('U', "30"), ('V', "31"), ('W', "32"), ('X',"33"), ('Y', "34"), ('Z',"35")]

examples = ["AL47 2121 1009 0000 0002 3569 8741", "AD12 0001 2030 2003 5910 0100", "AT61 1904 3002 3457 3201", "AZ21 NABZ 0000 0000 1370 1000 1944", "BH67 BMAG 0000 1299 1234 56"]

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

-- Test question 1 -- 
  
testQuestion1 :: [Integer] -> IO ()
testQuestion1 x = print ("Test on: " ++ show x ++ "Result" ++ show(triangle (x !! 0) (x !! 1) (x !! 2))) 	
					   					  

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
					  

--Random Integer	
getRandomInt :: Int -> IO Int 
getRandomInt n = getStdRandom (randomR (0,n))

randomFlip :: Int -> IO Int 
randomFlip x = do 
		b <- getRandomInt 1
		if b==0 then return x else return (-x)

genIntList :: IO [Int]
genIntList = do 
  k <- getRandomInt 20
  n <- getRandomInt 3
  getIntL k n
 
getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do 
   x <-  getRandomInt k
   y <- randomFlip x
   xs <- getIntL k (n-1)
   return (y:xs)

--f = triangle needs to be replaced by the genIntList but problem with IO versus Integer... :X   
testR :: Int -> Int -> (Integer -> Integer -> Integer -> Shape) -> IO ()
testR k n f = if k /= n then  
						do print ("Result" ++ show(f 3 3 3)) 	
						   testR (k+1) n f
					  else print (show n ++ " tests passed") 
                  				  
testPost :: (Integer -> Integer -> Integer -> Shape) -> IO ()
testPost f = testR 1 100 f