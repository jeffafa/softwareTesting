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

--valueByKeyX :: Boy -> Sentence -> Bool
--valueByKeyX _ [] = False
--valueByKeyX b (x:xs) = if (fst x) == b then snd x else valueByKey b xs

valueBy :: Boy -> Boy -> [Bool]
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

