import Data.List
import Data.Char

type Name = Int
type Valuation = [(Name,Bool)]
type ValFct = Name -> Bool

type Parser a b = [a] -> [(b,[a])]

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form 
          | Equiv Form Form 
          deriving Eq
		  
data Token 
      = TokenNeg
      | TokenCnj
      | TokenDsj
      | TokenImpl
      | TokenEquiv 
      | TokenInt Int 
      | TokenOP
      | TokenCP
	  deriving (Show,Eq)

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

--Assigement 3 forms
form12 = Impl (Dsj [Impl p q, Impl q r]) (Impl p r)

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

--Opdracht 2--
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs) 
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs 
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

parseForm :: Parser Token Form 
parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) = 
  [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) = 
  [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) = 
  [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
                         (f2,rest) <- parseImpl ys ]
   ++
  [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
                          (f2,rest) <- parseEquiv ys ] 
parseForm tokens = []

parseForms :: Parser Token [Form] 
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens = 
   [(f:fs, rest) | (f,ys) <- parseForm tokens, 
                   (fs,rest) <- parseForms ys ]
				   
parseImpl :: Parser Token Form
parseImpl (TokenImpl : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseImpl tokens = []

parseEquiv :: Parser Token Form
parseEquiv (TokenEquiv : tokens) = 
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseEquiv tokens = []

parse :: String -> [Form]
parse s = [ f | (f,_) <- parseForm (lexer s) ]

--Opdracht 2 code--
testdata = ["*(1 +(2 - 3))","*(1 +(2 - 3)","*(1 +(212312312- 3xx))"]

testV :: [String] -> IO ()
testV (x:xs) = if testX x == True then
                    do print ("pass on: " ++ show x)
                       testV xs
                   else error ("failed test on: " ++ show x) 

testX :: String -> Bool
testX f = if [ v | v <- show (parse f) ] == "[]" then False else True

--Opdracht 3--

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


