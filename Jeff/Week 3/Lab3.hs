module Lab3 where

import Data.List
import System.Random
import Lecture3

--Opdracht 1--
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (\ v -> not (evl v f)) (allVals f)

entails :: Form -> Form -> Bool 
entails f1 f2 = compareLists (evlTruthtable f1) (evlTruthtable f2)

compareLists :: [Bool] -> [Bool] -> Bool
compareLists [] [] = True
compareLists (x:xs) (y:ys) = if x == True && y == False then False else compareLists xs ys

equiv :: Form -> Form -> Bool
equiv f1 f2 = f (allVals f1) (allVals f2)

f :: [Valuation] -> [Valuation] -> Bool
f f1 f2 = if f1 == f2 then True else False

evlTruthtable :: Form -> [Bool]
evlTruthtable f = truthtable (allVals f) f

truthtable :: [Valuation] -> Form -> [Bool]
truthtable [] _ = []
truthtable (x:xs) f = evl x f : truthtable xs f 