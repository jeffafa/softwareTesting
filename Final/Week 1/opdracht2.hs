data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

data Matrix = Matrix [[Bool]]

data Sentence = Sentence [(Boy,Bool)]

--Logical sentences
sentencesMatthew = [(Matthew, False), (Peter, True), (Jack, True), (Arnold, True), (Carl,False)]
sentencesPeter= [(Matthew, True), (Peter, False), (Jack, True), (Arnold, False), (Carl,False)]
sentencesJack = [(Matthew, False), (Peter, False), (Jack, False), (Arnold, False), (Carl,True)]
sentencesArnold = [(Matthew, True), (Peter, True), (Jack, True), (Arnold, True), (Carl,False)]
sentencesCarl = [(Matthew, False), (Peter, False), (Jack, False), (Arnold, False), (Carl,True)]

f sentencesMatthew Matthew 

f :: [Sentence] -> Boy -> [Bool]
f [] = []
f xs Matthew,Peter,Jack,Arnold,Carl = map says xs

--f :: [Sentence] -> Boy -> Bool
--f (x:xs) b = if fst x == b then snd x else f(xs ,b)
--f [] = false

--Encoding of each sentence
says :: Boy -> Boy -> Bool
says Matthew = f sentencesMatthew, b  


-- f matthew en zijn sentence
-- says aanroepen en zijn boy bijhouden
-- matrix uitlezen en alleen accusers (true) der uithalen
-- dan guily checken enzo



accusers :: Boy -> [Boy]
accusers Matthew =  ( f om alles te bereken met die keys) says -de lijst van boys- matthew  

guilty :: [Boy]
guilty = []

honest :: [Boy]
honest = []


--Old solution

boysString = ["Carl", "Matthew", "Peter", "Jack", "Arnold", "Carl"]
booleanString = ["didnt ", "did"] 

filterSentence :: String -> [Bool]
filterSentence s = filterX (filterWords s)

filterX ::[String] -> [Bool]
filterX [] = []
filterX xs = map mapWord xs 

filterWords :: String -> [String]
filterWords str = filter acceptableWord (words str)
  where
    acceptableWord = all (`elem` "Carl and didnt neither did I")
	
mapWord x = mapWordToBool x -- if x elem boysString then mapWordToPerson else mapWordToBool 
	
mapWordToBool :: String -> Bool
mapWordToBool "didnt" = False
mapWordToBool "did" = True
mapWordToBool x = True

mapWordToPerson :: String -> Boy
mapWordToPerson "Carl" = Carl


----- oplossing 2?

--matthew    
    --Matthew false
    --Peter true
    --Jack true
    --Arnold true
    --Carl false

--Peter
    -- Matthew true
    --Peter false
    --Jack true
    -- Arnold false
    --Carl false
    
--Jack carl en matthew, matthew of jack
    --Matthew true/false ? tegenstrijdig false
    --Peter false/true ? tegenstrijdig`false
    --Jack False
    -- arnold false/true ? tegenstrijdig false
    --carl true
    
--Arnold
    --matthew or Peter hoe te vergelijken? Moeten we dan 
    --true
    --true
    -- true
    -- true
    -- false
    
--Carl moet deze niet inverse van arnold zijn?
    -- !matthew or !Peter hoe te vergelijken?
    -- true    false true 
    -- false true true
    -- false false false
    -- false true true
    -- true true     true