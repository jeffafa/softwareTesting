import Data.List
import System.Random
import Lecture3
import Lab3
--begin data
data FormTrait = Satisfiability | Tautology | Contradiction | entailment | equivalence 
					deriving (Eq,Show)
--end data

--begin types
type FormValuation = (FormTrait,Bool)
--end types

--begin TestData
--test Forms
form6 = Neg (Cnj[p,q])
form7 = Dsj[Neg p, Neg q]
form8 = Neg (Dsj[p,q])  
form9 = Cnj[p,q]
form10 = Dsj [Impl p q, Impl q p]
form11 = Neg p

--end TestData

--Begin TestFunctions
--Assignement1
testAssignement1 :: Form -> IO
testAssignement1 f = True

--Assignement2

--Assignement3

--Assignement4

--End TestFunctions