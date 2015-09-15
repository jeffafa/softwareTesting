import Data.List
import Data.Char

type Name = Int
type Valuation = [(Name,Bool)]
type ValFct = Name -> Bool

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form 
          | Equiv Form Form 
          deriving Eq

--Example formulas--
p = Prop 1
q = Prop 2
r = Prop 3 

--Test data
form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)
form4 = Neg (Neg (Neg p))
form5 = Cnj [p, Neg p]

form6 = Neg (Cnj[p,q])
form7 = Dsj[Neg p, Neg q]

form8 = Neg (Dsj[p,q])  
form9 = Cnj[p,q]
form10 = Dsj [Impl p q, Impl q p]
form11 = Neg p

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z 

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates = foldl update 

instance Show Form where 
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f 
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>" 
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>" 
                            ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

-- | all possible valuations for lists of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) = 
  map ((name,True) :) (genVals names)
  ++ map ((name,False):) (genVals names)

-- | generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

val2fct :: Valuation -> ValFct
val2fct = updates (\ _ -> undefined)

fct2val :: [Name] -> ValFct -> Valuation
fct2val domain f = map (\x -> (x,f x)) domain

evl :: Valuation -> Form -> Bool
evl [] (Prop c)    = error ("no info: " ++ show c)
evl ((i,b):xs) (Prop c)
     | c == i    = b
     | otherwise = evl xs (Prop c)
evl xs (Neg f)  = not (evl xs f)
evl xs (Cnj fs) = all (evl xs) fs
evl xs (Dsj fs) = any (evl xs) fs
evl xs (Impl f1 f2) = 
    not (evl xs f1) || evl xs f2
evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2

propNames :: Form -> [Name]
propNames = sort.nub.pnames where 
  pnames (Prop name) = [name]
  pnames (Neg f)  = pnames f
  pnames (Cnj fs) = concat (map pnames fs)
  pnames (Dsj fs) = concat (map pnames fs)
  pnames (Impl f1 f2)  = concat (map pnames [f1,f2])
  pnames (Equiv f1 f2) = concat (map pnames [f1,f2])

--Opdracht 1--
satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (\ v -> not (evl v f)) (allVals f)

entails :: Form -> Form -> Bool 
entails f1 f2 = compareLists (evlTruthtable f1) (evlTruthtable f2)

compareLists :: [Bool] -> [Bool] -> Bool
compareLists [] _ = True
compareLists _ [] = True
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

