module Week1

where 

import Data.List

factorial :: Integer -> Integer 
factorial n = product [1..n]

sentence = "Sentences can go " ++ onAndOn

onAndOn = "on and " ++ onAndOn

sentences = "Sentences can go on" : 
              map (++ " and on") sentences

sqr :: Int -> Int 
sqr = \ x -> x * x 

aword :: [Char] -> Bool
aword [] = False 
aword (x:xs) = (x == 'a') || (aword xs)

reversal :: [a] -> [a]
reversal []    = []
reversal (x:t) = reversal t ++ [x]

someEvens    = [ x | x <- [1..1000], even x ]

evensUntil n = [ x | x <- [1..n], even x ]

allEvens     = [ x | x <- [1..], even x ]

sonnet73 =
 "That time of year thou mayst in me behold\n"
 ++ "When yellow leaves, or none, or few, do hang\n"
 ++ "Upon those boughs which shake against the cold,\n"
 ++ "Bare ruin'd choirs, where late the sweet birds sang.\n"
 ++ "In me thou seest the twilight of such day\n"
 ++ "As after sunset fadeth in the west,\n"
 ++ "Which by and by black night doth take away,\n"
 ++ "Death's second self, that seals up all in rest.\n"
 ++ "In me thou see'st the glowing of such fire\n"
 ++ "That on the ashes of his youth doth lie,\n"
 ++ "As the death-bed whereon it must expire\n"
 ++ "Consumed with that which it was nourish'd by.\n"
 ++ "This thou perceivest, which makes thy love more strong,\n"
 ++ "To love that well which thou must leave ere long."

count :: Eq a => a -> [a] -> Int
count x []                 = 0 
count x (y:ys) | x == y    = succ (count x ys) 
               | otherwise = count x ys 

average :: [Int] -> Rational 
average [] = error "empty list" 
average xs = toRational (sum xs) / toRational (length xs)

data Creature = Lady | Tiger deriving (Eq,Show)

sign1, sign2 :: (Creature,Creature) -> Bool
sign1 (x,y) = x == Lady && y == Tiger
sign2 (x,y) = x /= y

solution1 :: [(Creature,Creature)]
solution1 = [ (x,y) | x <- [Lady,Tiger], 
                      y <- [Lady,Tiger], 
                      sign1 (x,y) /= sign2 (x,y) ]

data Islander = Knight | Knave deriving (Eq,Show)

john :: (Islander,Islander) -> Bool
john (x,y) = (x,y) == (Knave,Knave)

solution3 :: [(Islander,Islander)]
solution3 = [(x,y) | x <- [Knight,Knave], 
                     y <- [Knight,Knave], 
                     john (x,y) == (x == Knight) ]

main = putStrLn (s ++ show s) 
  where s = "main = putStrLn (s ++ show s) \n  where s = "

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve (filter (\ y -> rem y x /= 0) xs)

primes = sieve [2..]

