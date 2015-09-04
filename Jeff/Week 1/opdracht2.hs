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

sentences = [sentencesMatthew, sentencesPeter, sentencesJack, sentencesArnold, sentencesCarl]

--needed to have some readability in whatDoTheySayAbout and creates ownly one place with static 
getSentence :: Boy -> Sentence
getSentence Matthew = sentencesMatthew
getSentence Peter = sentencesPeter
getSentence Jack = sentencesJack
getSentence Arnold = sentencesArnold
getSentence Carl = sentencesCarl

--Could be fixed with getsentence
valueByKey :: Boy -> Sentence -> Bool
valueByKey _ [] = False
valueByKey b (x:xs) = if (fst x) == b then snd x else valueByKey b xs

getTulpe :: Boy -> Sentence -> (Boy,Bool)
getTulpe b (x:xs) = if (fst x) == b then x else getTulpe b xs

--Encoding of each sentence
says :: Boy -> Boy -> Bool
says Matthew b = (valueByKey b sentencesMatthew)
says Peter b = (valueByKey b sentencesPeter)
says Jack b = (valueByKey b sentencesJack)
says Arnold b = (valueByKey b sentencesArnold)
says Carl b = (valueByKey b sentencesCarl)

--accusers :: Boy -> [Boy]
--accusers b =  map (\x -> if (even x) then (x `div` 2) else x) [1,2,3,4]  

guilty :: [Boy]
guilty = []

honest :: [Boy]
honest = []

--Returns a sentence of what other people say about the boy
whatDoTheySayAbout :: Boy -> Sentence
whatDoTheySayAbout a = map(\xs -> toTuple xs (valueByKey a (getSentence xs))) boys

--needed to have some readability in whatDoTheySayAbout
toTuple :: Boy -> Bool -> (Boy,Bool)
toTuple a b = (a,b) 




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