module Lab3 where

import Data.List
import System.Random
import Lecture3

--Opdracht 1--
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (\ v -> not (evl v f)) (allVals f)