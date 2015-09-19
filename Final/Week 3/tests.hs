import Data.List
import System.Random
import Lecture3
import Lab3
import Data.Char
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import System.Random
import System.IO.Unsafe

--begin TestData
--test Forms
form6 = Neg (Cnj[p,q])
form7 = Dsj[Neg p, Neg q]
form8 = Neg (Dsj[p,q])  
form9 = Cnj[p,q]
form10 = Dsj [Impl p q, Impl q p]
form11 = Neg p

--end TestData

--Test Functions (assignment 2)
--To run both test for assignment 2
testAssignment2 :: IO()
testAssignment2 = do 
                   runTestParse 100
                   runTestParseShow 100
                   
--Test for Assignment 2 To Check the logical equivelant of the form going in and comming out.
runTestParse :: Int -> IO ()
runTestParse 0 = print ("Done")
runTestParse x = 
 do 
  testParse (formWithoutIO (randomForm (randomInt 0 5))) 
  runTestParse (x -1)

--Test for assignment 2 To compare output from parser with show
runTestParseShow :: Int -> IO ()
runTestParseShow 0 = print ("Done")
runTestParseShow x = 
 do 
  testV (show(formWithoutIO (randomForm (randomInt 0 5))))
  runTestParseShow (x -1)

--Test Functions (assignment 4)
testAssignment4 :: Int -> IO ()
testAssignment4 0 = print ("Done")
testAssignment4 x = 
 do 
  testCnf (formWithoutIO(randomForm (randomInt 0 5)))
  testAssignment4 (x -1)

--Random generators
randInRange :: Int -> Int -> IO Int
randInRange a b = getStdRandom $ randomR (a, b)

randomForm :: Int -> IO Form
randomForm 0 = do m <- randInRange 0 10
                  return (Prop m)
randomForm x = do y <- randInRange 0 3
                  case x of 0 -> do i <- randInRange 0 10
                                    return (Prop i)
                            1 -> do f <- randomForm (x -1)
                                    return (Neg f)
                            2 -> do f <- randomForm (x -1)
                                    x <- randomForm (x -1)
                                    return (Cnj [f, x])
                            3 -> do f <- randomForm (x -1)
                                    x <- randomForm (x -1)
                                    return (Dsj [f, x])
                            4 -> do f <- randomForm (x -1)
                                    x <- randomForm (x -1)
                                    return (Impl f x)
                            5 -> do f <- randomForm (x -1)
                                    x <- randomForm (x -1)
                                    return (Equiv f x)   					 

randomInt :: Int -> Int -> Int
randomInt x y = unsafePerformIO (randInRange x y) 					 

--IO Removers
formWithoutIO :: IO Form -> Form
formWithoutIO x = unsafePerformIO x

  --Single test functions
testV :: String -> IO ()
testV x = if testX x == True then print ("pass on: " ++ show x) else print ("failed test on: " ++ show x) 

testX :: String -> Bool
testX f = if [ v | v <- show (parse f) ] == "[]" then False else True

testParse :: Form -> IO ()
testParse x = if testParseEquiv x == True then print ("Passed:" ++ (show x) ) else print ("Failed:" == (show x)) 

testCnf :: Form -> IO ()
testCnf x = if equiv x (cnf x) == True then print ("Passed:" ++ (show x) ++ "and" ++ (show(cnf x)) ) else print ("Failed:" ++ (show x) ++ "and" ++ (show(cnf x))) 

testParseEquiv :: Form -> Bool
testParseEquiv x = equiv (head (parse (show x))) x			 
