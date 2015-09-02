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

--Says returns what a boys says about a boy
--So says Matthew Jack returns wheither Matthew accusses Jack or not in this True , so Matthew accusses Jack
--Encoding of each sentence
says :: Boy -> Boy -> Bool
says Matthew b = (valueByKey b sentencesMatthew)
says Peter b = (valueByKey b sentencesPeter)
says Jack b = (valueByKey b sentencesJack)
says Arnold b = (valueByKey b sentencesArnold)
says Carl b = (valueByKey b sentencesCarl)

--iterateSentences :: [Boy] -> Boy -> [Boy]
--iterateSentences [] _ = []
--iterateSentences (x:xs) y = if (says y x) == True then map x else iterateSentences xs y 
--iterateSentences (x:xs) y =  filter(says x y) 