module Lab6 where
 
import Data.List
import System.Random

--Assignment 1 Start with exp of 2, calculate first the previous result of exp 1. 
exM :: Integer -> Integer -> Integer -> Integer
exM base target modBy  | n == target = exMuP base (baseExponentOf1 base modBy) 2 n modBy
					   | otherwise = mod ((exMuP base (baseExponentOf1 base modBy) 2 n modBy) * (rem (base^(target-n)) modBy)) modBy
						 where n = exponentOf2 target


exMuP :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
exMuP base lastResult current target modBy | current == target = (lastResult^2 `mod` modBy)  
										   | otherwise = exMuP base (lastResult^2 `mod` modBy) (current*2) target modBy 
							

baseExponentOf1 base modBy = base^1 `mod` modBy
exponentOf2 x = last $ takeWhile (\y -> x >= y) [2^x | x <- [1..]]							
							
--Assignment 2 -- Especially on bigger numbers a difference can be
testEx :: (Integer -> Integer -> Integer -> Integer) -> (Integer -> Integer -> Integer -> Integer) -> IO()
testEx f1 f2 = do
      print $ ("f1 1 2 30: " ++ show (f1 5 40 7))
      print $ ("f2 1 2 30: " ++ show (f2 5 40 7))
      print "-Big numbers--"
      print $ ("f1 10 3000 20: " ++ show (f1 10 3000 20))
      print $ ("f2 10 3000 20: " ++ show (f2 10 3000 20))
      print "-Really big numbers-"
      print $ ("f1 323 67108895 1024: " ++ show (f1 2 582137556 1043))
      print $ ("f2 323 67108895 1024: " ++ show (f2 2 582137556 1043))

--Assignment 3
composites = 1 : filter isComposite [4..]
isComposite n = not (isPrime n)

--Assignment 4 Stuck on IO
printResult :: [IO Integer]
printResult = testCF composites

testCF :: [Integer] -> [IO Integer]
testCF (x:xs) = testCompsList' x : testCF xs

testCompsList' :: Integer -> IO Integer
testCompsList' x = do 
   a <- prime_test_F x
   return (if a then x else x)
   
prime_test_F :: Integer -> IO Bool
prime_test_F n = do 
   a <- randomRIO (1, n-1) :: IO Integer
   return (exM a (n-1) n == 1)		

--Assignment 5

--show list of primes in carmichaeal list using Fermat
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
      k <- [2..], 
      isPrime (6*k+1), 
      isPrime (12*k+1), 
      isPrime (18*k+1) ]
	  
	  
--Assignment 6 -- Started... similar implementation as assignment 7
testMR  :: Int -> IO [Integer]
testMR  n = listMR n carmichael []

listMR :: Int -> [Integer] -> [Integer] -> IO [Integer]
listMR 0 car ms = return ms
listMR n (c:car) ms = do 
							 isM <- primeMR 10 c
							 if isM
							    then listMR (n-1) car (c:ms)
								else 
									listMR n car ms  

--Assignment 7 -- Working but needed to change primeMR exM to expM because there is a bug in exM (assignment 1)
findMersenne  :: Int -> IO [Integer]
findMersenne  n = findMersenne' n primes []

findMersenne' :: Int -> [Integer] -> [Integer] -> IO [Integer]
findMersenne' 0 ps ms = return ms
findMersenne' n (p:ps) ms = do 
							 isM <- primeMR 10 (2^p - 1)
							 if isM
							    then findMersenne' (n-1) ps ((2^p - 1):ms)
								else 
									findMersenne' n ps ms
													    
--Week6 code
expM ::  Integer -> Integer -> Integer -> Integer
expM x y = rem (x^y)
   
decomp :: Integer -> (Integer,Integer)
decomp n = decomp' (0,n) where
  decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))
   
primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = let 
   (r,s) = decomp (n-1) 
   f = \ x -> takeWhile (/= 1) 
       (map (\ j -> expM x (2^j*s) n)  [0..r])
  in 
   do 
    a <- randomRIO (1, n-1) :: IO Integer
    if expM a (n-1) n /= 1 
      then return False 
      else 
        if expM a s n /= 1 && last (f a) /= (n-1) 
          then return False
          else primeMR (k-1) n

factors :: Integer -> [Integer]
factors n = let 
   ps = takeWhile (\m -> m^2 <= n) primes
 in factors' n ps where 
   factors' 1 _  = []
   factors' n [] = [n]
   factors' n (p:ps) 
    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
    | otherwise      =    factors' n ps

isPrime n = factors n == [n]
primes = 2 : filter isPrime [3..]

primes' = sieve [2..]
sieve (n:ns) = n : sieve 
   (filter (\ m -> rem m n /= 0) ns)