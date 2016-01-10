module ST where 

import Test.Hspec
import Test.QuickCheck
import System.Random
import Data.List

data Color = W | B deriving (Eq,Show)

drawPebble :: [Color] -> [Color]
drawPebble [] = []
drawPebble [x] = [x]
drawPebble (W:W:xs) = drawPebble (B:xs) 
drawPebble (B:B:xs) = drawPebble (B:xs) 
drawPebble (W:B:xs) = drawPebble (W:xs) 
drawPebble (B:W:xs) = drawPebble (W:xs) 

instance Arbitrary Color where
   arbitrary = oneof [return W, return B]

numberW :: [Color] -> Int
numberW = length . (filter (== W)) 

parityW :: [Color] -> Int
parityW xs =  mod (numberW xs) 2

prop_invariant xs = 
  parityW xs == parityW (drawPebble xs)

prop_length xs = length xs == length (drawPebble xs)

evens1 = [ n | n <- [0..], even n ] 

evens2 = [ 2*n | n <- [0..] ]

naturals = [0..] 
small_squares1 = [ n^2 | n <- [0..999] ]
small_squares2 = [ n^2 | n <- naturals , n < 1000 ]
small_squares3 = take 1000 [ n^2 | n <- naturals ]

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs) 
                     ++ (map (x:) (powerList xs))

