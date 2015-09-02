data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
type Sentence = [(Boy,Bool)]

boys = [Matthew, Peter, Jack, Arnold, Carl]
--Logical sentences
sentencesMatthew = [(Matthew, False), (Peter, True), (Jack, True), (Arnold, True), (Carl,False)]
sentencesPeter= [(Matthew, True), (Peter, False), (Jack, True), (Arnold, False), (Carl,False)]
sentencesJack = [(Matthew, False), (Peter, False), (Jack, False), (Arnold, False), (Carl,True)]
sentencesArnold = [(Matthew, True), (Peter, True), (Jack, True), (Arnold, True), (Carl,False)]
sentencesCarl = [(Matthew, False), (Peter, False), (Jack, False), (Arnold, False), (Carl,True)]

valueByKey :: Boy -> Sentence -> Bool
valueByKey _ [] = False
valueByKey b (x:xs) = if (fst x) == b then snd x else valueByKey b xs

--Encoding of each sentence
says :: Boy -> Boy -> Bool
says Matthew b = (valueByKey b sentencesMatthew)
says Peter b = (valueByKey b sentencesPeter)
says Jack b = (valueByKey b sentencesJack)
says Arnold b = (valueByKey b sentencesArnold)
says Carl b = (valueByKey b sentencesCarl)

accusers :: Boy -> [Boy]
accusers b =  if (says )  

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