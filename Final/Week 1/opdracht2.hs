data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
type Sentence = [(Boy,Bool)]

boys = [Matthew, Peter, Jack, Arnold, Carl]
--boys = [Matthew, Jack]
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

accusers :: Boy -> [Boy]
accusers b = map fst (filter ((==True).snd) (whatDoTheySayAbout b)) 

--Need to have similar like honest function but instead get JACK and not the 3 guys who told the truth
guilty :: [Boy]
guilty = giveWhere (\x -> numberOfAccusers x >= 3) boys
								
giveWhere :: (a -> Bool) -> [a] -> [a]
giveWhere _ [] = []
giveWhere x (y:ys) | x y	= y : giveWhere x ys
				   | otherwise = giveWhere x ys 

numberOfAccusers :: Boy -> Int
numberOfAccusers a = length (accusers a)

--Returns the people who told the truth about the guilty guy
honest :: [Boy]
honest = concat (filter (\x -> length x == 3)(map accusers boys))

--Returns a sentence of what other people say about the boy
whatDoTheySayAbout :: Boy -> Sentence
whatDoTheySayAbout a = map(\xs -> toTuple xs (valueByKey a (getSentence xs))) boys

--needed to have some readability in whatDoTheySayAbout
toTuple :: Boy -> Bool -> (Boy,Bool)
toTuple a b = (a,b) 