module QuickArbit

where

import Data.List
import Test.QuickCheck
import Control.Monad

jeff:: Int -> Bool
jeff _ = True

data Bier
    = Heineken Int
    | Chang Int
    | Bintang Int
    | Bocht Int
    | Meerdere Bier Bier
    | HeulVeul [Bier]
    deriving (Eq,Show)
      
prop_genoeg_bier :: Bier -> Bool
prop_genoeg_bier (Heineken i) = i >= 5
prop_genoeg_bier (Chang i) = i >= 5
prop_genoeg_bier (Bintang i) = i >= 5
prop_genoeg_bier (Bocht i) = i >= 10
prop_genoeg_bier (Meerdere b1 b2) = (prop_genoeg_bier b1) && (prop_genoeg_bier b2)
prop_genoeg_bier (HeulVeul []) = True
prop_genoeg_bier (HeulVeul (b:bs)) = (prop_genoeg_bier b) && (prop_genoeg_bier (HeulVeul bs))

instance Arbitrary Bier where
    arbitrary = gbier

gbier = sized gbier'

gbier' 0 = liftM Heineken (choose (6,15))
gbier' n | n > 15 = gbier' (n `div` 2)           
gbier' n | n > 0 =
           oneof [liftM Chang (choose (6,15)),
                  liftM Bintang (choose (6,15)),
                  liftM Bocht (choose (11,15)),
                  liftM2 Meerdere biertje biertje,
                  liftM HeulVeul biertjes]
           where biertje = gbier' (n `div` 2)
                 biertjes = resize (n `div` 2) (vector (n `div` 2))
                 
verboseAll :: IO ()
verboseAll = do verboseCheck prop_genoeg_bier

quickAll :: IO ()
quickAll = do quickCheck prop_genoeg_bier