data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
type Sentence = [(Boy,Boy,Bool)]

boys = [Matthew, Peter, Jack, Arnold, Carl]

--Logical sentences
sentencesMatthew = [(Matthew,Matthew,False), (Matthew,Peter, True), (Matthew,Jack, True), (Matthew,Arnold, True), (Matthew,Carl,False)]
sentencesPeter= [(Peter,Matthew, True), (Peter,Peter, False), (Peter,Jack, True), (Peter,Arnold, False), (Peter,Carl,False)]
sentencesJack = [(Jack,Matthew, False), (Jack,Peter, False), (Jack,Jack, False), (Jack,Arnold, False), (Jack,Carl,True)]
sentencesArnold = [(Arnold,Matthew, True), (Arnold,Peter, True), (Arnold,Jack, True), (Arnold,Arnold, True), (Arnold,Carl,False)]
sentencesCarl = [(Carl,Matthew, False), (Carl,Peter, False), (Carl,Jack, False), (Carl,Arnold, False), (Carl,Carl,True)]

sentences = [sentencesMatthew, sentencesPeter, sentencesJack, sentencesArnold, sentencesCarl]

--Encoding of each sentence
says :: Boy -> Boy -> [Bool]
says a b = valueBy a b

valueBy :: Boy -> Boy -> Bool
valueBy a b = map (valueByKey a b) sentences   

valueByKey :: Boy -> Boy -> Sentence -> Bool
valueByKey _ _ [] = False
valueByKey a b (x:xs) = if (fst3 x) == a && (snd3 x) == b then thr3 x else valueByKey a b xs

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thr3 :: (a,b,c) -> c
thr3 (_,_,c) = c

accusers :: Boy -> [Boy]
accusers b = []

--Correct but need get the boy not only the bool
f :: Boy -> Bool --[Boy]
f a = False
--f b = map (valueByKey b) sentences 

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