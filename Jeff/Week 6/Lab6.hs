module Lab6 where
 
  import Data.List
  import System.Random
  import Data.Bits
  import Lecture6
  import Data.Char
  --Needed for testing
  import Control.Exception
  import Data.Time
  import Test.QuickCheck
  import Text.Printf
  import System.CPUTime
  import System.IO.Unsafe
  
--Exercise1 c=4^13 (mod 497) | c = b^e (mod m)
-- Each method is described on wikipedia (https://en.wikipedia.org/wiki/Modular_exponentiation)

--        b         e           m         c Straightforward Method
  exMFM :: Integer -> Integer -> Integer -> Integer
  exMFM b 0 m = 1
  exMFM a b c = (a^b) `mod` c

  --      b           e         m           c Memory-efficient method (at least I think so...)
  exMME :: Integer -> Integer -> Integer -> Integer
  exMME b 0 m = 1
  exMME b e m = exMME' b e m 1 0 where    
    exMME' b e m c e'        
      | e' < e = exMME' b e m ((b * c) `mod` m) (e'+1)
      | otherwise = c
      
--         b           e         m           c Right-to-left binary method
  exMRL :: Integer -> Integer -> Integer -> Integer
  exMRL b 0 m = 1
  exMRL b e m = t * exMRL ((b * b) `mod` m) (shiftR e 1) m `mod` m
                     where t = if testBit e 0 then b `mod` m else 1     

--Exercise2 
  exercise2 :: IO ()
  exercise2 = do
                print ("Comparing answers of exM with exMfM: ")
                quickCheck prop_CompareAnswerExMFM
                print ("Comparing answers of exM with exMME: ")
                quickCheck prop_CompareAnswerExMME
                print ("Comparing answers of exM with exMRL: ")
                quickCheck prop_CompareAnswerExMRL
                --print ("benching exM with exMRL: ")
                --quickCheck prop_Bench
  
  prop_CompareAnswerExMFM :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
  prop_CompareAnswerExMFM (Positive b) (Positive e) (Positive m) = exMFM b e m == exM b e m 
  
  prop_CompareAnswerExMME :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
  prop_CompareAnswerExMME (Positive b) (Positive e) (Positive m) = exMME b e m == exM b e m

  prop_CompareAnswerExMRL :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> Bool
  prop_CompareAnswerExMRL (Positive b) (Positive e) (Positive m) = exMRL b e m == exM b e m   

  prop_Bench :: (Positive Integer) -> (Positive Integer) -> (Positive Integer) -> IO ()
  prop_Bench (Positive b) (Positive e) (Positive m) = bench b e m    
  
  bench :: Integer -> Integer -> Integer -> IO ()
  bench b e m= do
    putStrLn "exM versus exMME"
    putStrLn "exM"
    bexM <- benchexM b e m
    print bexM
    putStrLn "exMME"
    beexMME <- benchexMME b e m
    print beexMME
  
  benchexMME :: Integer -> Integer -> Integer -> IO NominalDiffTime
  benchexMME b e m = do
    start <- getCurrentTime
    evaluate (exMME b e m)
    end <- getCurrentTime
    return (diffUTCTime end start)

  benchexM :: Integer -> Integer -> Integer -> IO NominalDiffTime
  benchexM b e m = do
    start <- getCurrentTime
    evaluate (exM b e m)
    end <- getCurrentTime
    return (diffUTCTime end start)

--Assignment 3
  composites = 1 : filter composite [4..] where
    composite n = not (isPrime n)

--Exercise 4
  ioIntToInt :: [IO Integer] -> [Integer]
  ioIntToInt (x:xs) = unsafePerformIO x : ioIntToInt xs
  
  ioBoolToBool :: IO Bool -> Bool
  ioBoolToBool x = unsafePerformIO x
  
  printResult :: [Integer]
  printResult = ioIntToInt(testCF composites)

  testCF :: [Integer] -> [IO Integer]
  testCF (x:xs) = testCompsList' x : testCF xs

  testCompsList' :: Integer -> IO Integer
  testCompsList' x = do 
    a <- prime_test_F x
    return (if a then x else x)
   
--Exercise 5   
  carmichael :: [Integer]
  carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    isPrime (6*k+1), 
    isPrime (12*k+1), 
    isPrime (18*k+1) ]
  
  testCarCF :: [Integer] -> [Integer]
  testCarCF [] = []
  testCarCF (x:xs) | ioBoolToBool(prime_test_F x) = x : testCarCF xs
                   | otherwise = testCarCF xs
                   
  carPrintResult :: [Integer]
  carPrintResult = testCarCF composites
  
  --Exercise 6
  millerAndRabin :: Integer -> Integer -> Bool
  millerAndRabin 0 _ = False
  millerAndRabin n x = if x^2 == (1 `mod` n) then millerAndRabin2 n x (1 `mod` n) else False

  millerAndRabin2 :: Integer -> Integer -> Integer -> Bool
  millerAndRabin2 n x a | x == a = True
                        | x == (a * (-1)) = True
                        | otherwise = False
                        
  testMaR :: [Integer] -> [(Integer, Bool)]
  testMaR (x:xs)  = (x , millerAndRabin x x) : testMaR xs
  
  maRCARPrintResults :: [(Integer, Bool)]
  maRCARPrintResults = testMaR carmichael

  maRCOMPrintResults :: [(Integer, Bool)]
  maRCOMPrintResults = testMaR composites  
  
