module Lab4 where
 
import Data.List
import System.Random
import Test.QuickCheck  

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a 

