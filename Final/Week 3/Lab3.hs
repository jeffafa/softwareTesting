module Lab3 where

import Data.List
import System.Random
import Lecture3

--Opdracht 1, Time spend: 2 hours.

--Check if all are True
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

--Check if all are False
contradiction :: Form -> Bool
contradiction f = all (\ v -> not (evl v f)) (allVals f)
 
entails :: Form -> Form -> Bool 
entails f1 f2 = compareLists (evlTruthtable f1) (evlTruthtable f2)

--Check if there is a T-F relationship between both formulas, if this is the case then it is not a entail
compareLists :: [Bool] -> [Bool] -> Bool
compareLists [] [] = True
compareLists (x:xs) (y:ys) = if x == True && y == False then False else compareLists xs ys

--Check if both truth tables are equiv 
equiv :: Form -> Form -> Bool
equiv f1 f2 = f (allVals f1) (allVals f2)

f :: [Valuation] -> [Valuation] -> Bool
f f1 f2 = if f1 == f2 then True else False

evlTruthtable :: Form -> [Bool]
evlTruthtable f = truthtable (allVals f) f

truthtable :: [Valuation] -> Form -> [Bool]
truthtable [] _ = []
truthtable (x:xs) f = evl x f : truthtable xs f 

--For Assignment 2 please see tests.hs, Time spend: 5 hours

--Assignment 3, Time spend: 8 hours, solution is different than the examples in the assignment but asked Bert Lisser if this was oké, and according to him it was no problem. 

--Generate only the false outcomes of the complete truth table
getFalseTruthTable :: [Valuation] -> Form -> [Valuation]
getFalseTruthTable [] _ = []
getFalseTruthTable (x:xs) f = if (evl x f) == False then x : getFalseTruthTable xs f else getFalseTruthTable xs f 

--Swap each p r q etc. from t to f or f to t
swapEachLiteral :: [Valuation] -> [Valuation]
swapEachLiteral [] = []
swapEachLiteral (x:xs) = swapEachAtom x : swapEachLiteral xs

swapEachAtom :: Valuation -> Valuation
swapEachAtom [] = []
swapEachAtom (x:xs) = if snd x == True then (fst x,False) : swapEachAtom xs else (fst x, True) : swapEachAtom xs

createForm :: [Valuation] -> [[Form]]
createForm [] = []
createForm (x:xs) = createFormVal x : createForm xs

createFormVal :: Valuation -> [Form]
createFormVal [] = []
createFormVal (x:xs) = if snd x == True then (Prop (fst x)) : createFormVal xs else Neg (Prop (fst x)) : createFormVal xs

--clause put the p q r in dsj 
clause :: [[Form]] -> [Form]
clause [] = []
clause (x:xs) = Dsj x : clause xs

--conjunction of clauses
conjunctionClause :: [Form] -> Form
conjunctionClause x = Cnj x

--Final output CNF
cnf :: Form -> Form 
cnf f =  conjunctionClause(clause(createForm(swapEachLiteral(getFalseTruthTable(allVals f) f))))