  module Lecture5
  
  where 
  
  import Data.List
  import Data.Ix
  import System.Random

  type Row    = Int 
  type Column = Int 
  type Value  = Int
  type Grid   = [[Value]]
  
  type Position = (Row,Column)
  type Constrnt = [[Position]]
  
  type Sudoku = (Row,Column) -> Value
   
  positions, values :: [Int]
  positions = [1..9]
  values    = [1..9] 
  
  blocks :: [[Int]]
  blocks = [[1..3],[4..6],[7..9]]
       
  --Show values in sudoku
  showVal :: Value -> String
  showVal 0 = " "
  showVal d = show d

  showRow :: [Value] -> IO()
  showRow [a1,a2,a3,a4,a5,a6,a7,a8,a9] = 
   do  putChar '|'         ; putChar ' '
       putStr (showVal a1) ; putChar ' '
       putStr (showVal a2) ; putChar ' '
       putStr (showVal a3) ; putChar ' '
       putChar '|'         ; putChar ' '
       putStr (showVal a4) ; putChar ' '
       putStr (showVal a5) ; putChar ' '
       putStr (showVal a6) ; putChar ' '
       putChar '|'         ; putChar ' '
       putStr (showVal a7) ; putChar ' '
       putStr (showVal a8) ; putChar ' '
       putStr (showVal a9) ; putChar ' '
       putChar '|'         ; putChar '\n'

  showGrid :: Grid -> IO()
  showGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
   do putStrLn ("+-------+-------+-------+")
      showRow as; showRow bs; showRow cs
      putStrLn ("+-------+-------+-------+")
      showRow ds; showRow es; showRow fs
      putStrLn ("+-------+-------+-------+")
      showRow gs; showRow hs; showRow is
      putStrLn ("+-------+-------+-------+")

--Sudoku functions to show	  
	  
  sud2grid :: Sudoku -> Grid
  sud2grid s = 
    [ [ s (r,c) | c <- [1..9] ] | r <- [1..9] ] 
  
  grid2sud :: Grid -> Sudoku
  grid2sud gr = \ (r,c) -> pos gr (r,c) 
    where 
    pos :: [[a]] -> (Row,Column) -> a 
    pos gr (r,c) = (gr !! (r-1)) !! (c-1)

  showSudoku :: Sudoku -> IO()
  showSudoku = showGrid . sud2grid

--Internal structure sudoku  
  
  bl :: Int -> [Int]
  bl x = concat $ filter (elem x) blocks 

  subGrid :: Sudoku -> (Row,Column) -> [Value]
  subGrid s (r,c) = [ s (r',c') | r' <- bl r, c' <- bl c ]
	
	
--Assignment 1 --Time spend: 7 hours	
--2,2, 2,3, 2,4  6,2, 6,3, 6,4
--3,2, 3,3, 3,4  7,2, 7,3, 7,4
--4,2, 4,3, 4,4  8,2, 8,3, 8,4

--2,6, 2,7, 2,8  6,6, 6,7, 6,8
--3,6, 3,7, 3,8  7,6, 7,7, 7,8
--4,6, 4,7, 4,8  8,6, 8,7, 8,8	
	
  nrcGrid :: Sudoku -> (Row,Column) -> [Value]
  nrcGrid s (r,c) | r `elem` [2..4] && c `elem` [2..4] = [ s (r',c') | r' <- [2..4], c' <- [2..4] ] 
				  | r `elem` [6..8] && c `elem` [2..4] = [ s (r',c') | r' <- [6..8], c' <- [2..4] ]
				  | r `elem` [2..4] && c `elem` [6..8] = [ s (r',c') | r' <- [2..4], c' <- [6..8] ]
				  | r `elem` [6..8] && c `elem` [6..8] = [ s (r',c') | r' <- [6..8], c' <- [6..8] ]
				  | r `elem` [1,5,9] = []
				  | c `elem` [1,5,9] = []

--Assignment 2 --Time spend: 4 hours, for NRC this solution is more easier to implement since it works which positions instead of static blocks

  rowConstrnt :: Constrnt
  rowConstrnt = [[(r,c)| c <- values ] | r <- values ]
  
  columnConstrnt :: Constrnt
  columnConstrnt = [[(r,c)| r <- values ] | c <- values ]
  
  blockConstrnt :: Constrnt
  blockConstrnt = [[(r,c)| r <- b1, c <- b2 ] | b1 <- blocks, b2 <- blocks ]
  
  freeAtPos' :: Sudoku -> Position -> Constrnt -> [Value]
  freeAtPos' s (r,c) xs = let 
     ys = filter (elem (r,c)) xs 
   in 
     foldl1 intersect (map ((values \\) . map s) ys)
	 
  freeAtPosition' :: Sudoku -> Position -> [Value]
  freeAtPosition' s (r,c) = 
   (freeAtPos' s (r,c) rowConstrnt) 
	`intersect` (freeAtPos' s (r,c) columnConstrnt) 
	`intersect` (freeAtPos' s (r,c) blockConstrnt) 	  
  
--Original code continue  
  
  freeInSeq :: [Value] -> [Value]
  freeInSeq seq = values \\ seq 

  freeInRow :: Sudoku -> Row -> [Value]
  freeInRow s r = freeInSeq [ s (r,i) | i <- positions  ]

  freeInColumn :: Sudoku -> Column -> [Value]
  freeInColumn s c = freeInSeq [ s (i,c) | i <- positions ]

  freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
  freeInSubgrid s (r,c) = freeInSeq (subGrid s (r,c))
  
  freeInNrcgrid :: Sudoku -> (Row,Column) -> [Value]
  freeInNrcgrid s (r,c) = freeInSeq (nrcGrid s (r,c))

  freeAtPos :: Sudoku -> (Row,Column) -> [Value]
  freeAtPos s (r,c) = 
    (freeInRow s r) 
     `intersect` (freeInColumn s c) 
     `intersect` (freeInSubgrid s (r,c)) 
	 `intersect` (freeInNrcgrid s (r,c))
	 
--Check for consistent between rows, columns etc.

  injective :: Eq a => [a] -> Bool
  injective xs = nub xs == xs
    
  rowInjective :: Sudoku -> Row -> Bool
  rowInjective s r = injective vs where 
     vs = filter (/= 0) [ s (r,i) | i <- positions ]

  colInjective :: Sudoku -> Column -> Bool
  colInjective s c = injective vs where 
     vs = filter (/= 0) [ s (i,c) | i <- positions ]

  subgridInjective :: Sudoku -> (Row,Column) -> Bool
  subgridInjective s (r,c) = injective vs where 
     vs = filter (/= 0) (subGrid s (r,c))
	 	 
  nrcgridInjective :: Sudoku -> (Row,Column) -> Bool
  nrcgridInjective s (r,c) = injective vs where 
     vs = filter (/= 0) (nrcGrid s (r,c)) 	 	 

  consistent :: Sudoku -> Bool
  consistent s = and $
                 [ rowInjective s r |  r <- positions ]
                  ++
                 [ colInjective s c |  c <- positions ]
                  ++
                 [ subgridInjective s (r,c) | 
                      r <- [1,4,7], c <- [1,4,7]]
                  ++
                 [ nrcgridInjective s (r,c) | 
                      r <- [2,6], c <- [2,6]]					  
					 

  extend :: Sudoku -> ((Row,Column),Value) -> Sudoku
  extend = update

  update :: Eq a => (a -> b) -> (a,b) -> a -> b 
  update f (y,z) x = if x == y then z else f x 

  type Constraint = (Row,Column,[Value])

  type Node = (Sudoku,[Constraint])
 
  showNode :: Node -> IO()
  showNode = showSudoku . fst

  solved  :: Node -> Bool
  solved = null . snd

  extendNode :: Node -> Constraint -> [Node]
  extendNode (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           prune (r,c,v) constraints) | v <- vs ]

  length3rd :: (a,b,[c]) -> (a,b,[c]) -> Ordering
  length3rd (_,_,zs) (_,_,zs') = compare (length zs) (length zs')

  prune :: (Row,Column,Value) 
        -> [Constraint] -> [Constraint]
  prune _ [] = []
  prune (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : prune (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : prune (r,c,v) rest
    | sameblock (r,c) (x,y) = 
          (x,y,zs\\[v]) : prune (r,c,v) rest
    | otherwise = (x,y,zs) : prune (r,c,v) rest
  
  sameblock :: (Row,Column) -> (Row,Column) -> Bool
  sameblock (r,c) (x,y) = bl r == bl x && bl c == bl y 

  initNode :: Grid -> [Node]
  initNode gr = let s = grid2sud gr in 
                if (not . consistent) s then [] 
                else [(s, constraints s)]

  openPositions :: Sudoku -> [(Row,Column)]
  openPositions s = [ (r,c) | r <- positions,  
                              c <- positions, 
                              s (r,c) == 0 ]

--Orginal
--  constraints :: Sudoku -> [Constraint] 
--  constraints s = sortBy length3rd 
--      [(r,c, freeAtPos s (r,c)) | 
--                         (r,c) <- openPositions s ]

--Code for assignment 2	
					 
  constraints :: Sudoku -> [Constraint] 
  constraints s = sortBy length3rd 
      [(r,c, freeAtPosition' s (r,c)) | 
                         (r,c) <- openPositions s ]

  
  data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

  exmple1 = T 1 [T 2 [], T 3 []]
  exmple2 = T 0 [exmple1,exmple1,exmple1]

  grow :: (node -> [node]) -> node -> Tree node 
  grow step seed = T seed (map (grow step) (step seed))

  count :: Tree a -> Int 
  count (T _ ts) = 1 + sum (map count ts)

  search :: (node -> [node]) 
         -> (node -> Bool) -> [node] -> [node]
  search children goal [] = []
  search children goal (x:xs) 
    | goal x    = x : search children goal xs
    | otherwise = search children goal ((children x) ++ xs)

  solveNs :: [Node] -> [Node]
  solveNs = search succNode solved 
  
  succNode :: Node -> [Node]
  succNode (s,[]) = []
  succNode (s,p:ps) = extendNode (s,ps) p 

  solveAndShow :: Grid -> IO[()]
  solveAndShow gr = solveShowNs (initNode gr)
  
  solveShowNs :: [Node] -> IO[()]
  solveShowNs = sequence . fmap showNode . solveNs

  example1 :: Grid
  example1 = [[5,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

  example2 :: Grid
  example2 = [[0,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9]]

  example3 :: Grid
  example3 = [[1,0,0,0,3,0,5,0,4],
              [0,0,0,0,0,0,0,0,3],
              [0,0,2,0,0,5,0,9,8], 
              [0,0,9,0,0,0,0,3,0],
              [2,0,0,0,0,0,0,0,7],
              [8,0,3,0,9,1,0,6,0],
              [0,5,1,4,7,0,0,0,0],
              [0,0,0,3,0,0,0,0,0],
              [0,4,0,0,0,9,7,0,0]]

  example4 :: Grid
  example4 = [[1,2,3,4,5,6,7,8,9],
              [2,0,0,0,0,0,0,0,0],
              [3,0,0,0,0,0,0,0,0],
              [4,0,0,0,0,0,0,0,0],
              [5,0,0,0,0,0,0,0,0],
              [6,0,0,0,0,0,0,0,0],
              [7,0,0,0,0,0,0,0,0],
              [8,0,0,0,0,0,0,0,0],
              [9,0,0,0,0,0,0,0,0]]

  example5 :: Grid
  example5 = [[1,0,0,0,0,0,0,0,0],
              [0,2,0,0,0,0,0,0,0],
              [0,0,3,0,0,0,0,0,0],
              [0,0,0,4,0,0,0,0,0],
              [0,0,0,0,5,0,0,0,0],
              [0,0,0,0,0,6,0,0,0],
              [0,0,0,0,0,0,7,0,0],
              [0,0,0,0,0,0,0,8,0],
              [0,0,0,0,0,0,0,0,9]]
			  
  nrcExample :: Grid
  nrcExample = [[0,0,0,3,0,0,0,0,0],
              [0,0,0,7,0,0,3,0,0],
              [2,0,0,0,0,0,0,0,8],
              [0,0,6,0,0,5,0,0,0],
              [0,9,1,6,0,0,0,0,0],
              [3,0,0,0,7,1,2,0,0],
              [0,0,0,0,0,0,0,3,1],
              [0,8,0,0,4,0,0,0,0],
              [0,0,2,0,0,0,0,0,0]]
			  
  nrcExample2 :: Grid
  nrcExample2 = [[0,0,3,0,0,0,0,8,0],
                 [0,0,0,5,0,0,0,0,0],
                 [0,0,6,0,0,0,0,0,0],
                 [0,4,0,0,2,0,0,0,0],
                 [0,0,0,0,4,6,0,0,0],
                 [1,0,0,0,0,0,0,3,0],
                 [0,0,0,8,0,0,4,9,2],
                 [0,0,0,0,0,0,0,0,0],
                 [5,6,0,0,0,0,0,0,0]]
				 
  nrcExample3 :: Grid
  nrcExample3 = [[8,3,2,0,0,0,1,4,5],
                 [7,6,4,0,8,0,9,0,0],
                 [9,0,0,3,0,0,0,0,0],
                 [3,0,0,0,0,5,4,0,1],
                 [1,0,0,0,0,0,0,0,0],
                 [5,0,0,2,0,0,0,0,8],
                 [4,0,0,0,0,6,0,7,0],
                 [6,5,0,0,0,0,0,0,0],
                 [2,0,0,0,3,0,0,1,0]]

  emptyN :: Node
  emptyN = (\ _ -> 0,constraints (\ _ -> 0))

  getRandomInt :: Int -> IO Int
  getRandomInt n = getStdRandom (randomR (0,n))

  getRandomItem :: [a] -> IO [a]
  getRandomItem [] = return []
  getRandomItem xs = do n <- getRandomInt maxi
                        return [xs !! n]
                     where maxi = length xs - 1

  randomize :: Eq a => [a] -> IO [a]
  randomize xs = do y <- getRandomItem xs 
                    if null y 
                      then return []
                      else do ys <- randomize (xs\\y)
                              return (head y:ys)

  sameLen :: Constraint -> Constraint -> Bool
  sameLen (_,_,xs) (_,_,ys) = length xs == length ys

  getRandomCnstr :: [Constraint] -> IO [Constraint]
  getRandomCnstr cs = getRandomItem (f cs) 
    where f [] = []
          f (x:xs) = takeWhile (sameLen x) (x:xs)

  rsuccNode :: Node -> IO [Node]
  rsuccNode (s,cs) = do xs <- getRandomCnstr cs
                        if null xs 
                          then return []
                          else return 
                            (extendNode (s,cs\\xs) (head xs))

  rsolveNs :: [Node] -> IO [Node]
  rsolveNs ns = rsearch rsuccNode solved (return ns)

  rsearch :: (node -> IO [node]) 
              -> (node -> Bool) -> IO [node] -> IO [node]
  rsearch succ goal ionodes = 
    do xs <- ionodes 
       if null xs 
         then return []
         else 
           if goal (head xs) 
             then return [head xs]
             else do ys <- rsearch succ goal (succ (head xs))
                     if (not . null) ys 
                        then return [head ys]
                        else if null (tail xs) then return []
                             else 
                               rsearch 
                                 succ goal (return $ tail xs)

  genRandomSudoku :: IO Node
  genRandomSudoku = do [r] <- rsolveNs [emptyN]
                       return r

  randomS = genRandomSudoku >>= showNode

  uniqueSol :: Node -> Bool
  uniqueSol node = singleton (solveNs [node]) where 
    singleton [] = False
    singleton [x] = True
    singleton (x:y:zs) = False

  eraseS :: Sudoku -> (Row,Column) -> Sudoku
  eraseS s (r,c) (x,y) | (r,c) == (x,y) = 0
                       | otherwise      = s (x,y)

  eraseN :: Node -> (Row,Column) -> Node
  eraseN n (r,c) = (s, constraints s) 
    where s = eraseS (fst n) (r,c) 

  minimalize :: Node -> [(Row,Column)] -> Node
  minimalize n [] = n
  minimalize n ((r,c):rcs) | uniqueSol n' = minimalize n' rcs
                           | otherwise    = minimalize n  rcs
    where n' = eraseN n (r,c)

  filledPositions :: Sudoku -> [(Row,Column)]
  filledPositions s = [ (r,c) | r <- positions,  
                                c <- positions, s (r,c) /= 0 ]

  genProblem :: Node -> IO Node
  genProblem n = do ys <- randomize xs
                    return (minimalize n ys)
     where xs = filledPositions (fst n)

--Assignment 3 -- 4 hours
  --If you have a sudoku with more then one solution it is not minimal . Keep removing hints until you get a sudoku with more then one solution and return the previous one.
  hasMultibleSolutions :: Grid -> Integer -> Bool
  hasMultibleSolutions s i = if length (searchT i succNode solved (initNode s)) > 1 then True else False
  
  searchT :: Integer -> (node -> [node]) 
         -> (node -> Bool) -> [node] -> [node]
  searchT i children goal [] = []
  searchT 0 _ _ _ = []
  searchT i children goal (x:xs) 
    | goal x    = x : searchT (i-1) children goal xs
    | otherwise = searchT (i) children goal ((children x) ++ xs)
	
  isMinimal :: Grid -> Bool
  isMinimal g = not (hasMultibleSolutions g 2)

  --testAssignment
  runTestMinimal :: Grid -> IO ()
  runTestMinimal [] = print ("Done")
  runTestMinimal x = do
					  y <- genRandomSudoku
					  if isMinimal x then print ("passed") else print ("Failed")

  --Assigement 4-- First idea: Generate random sudoko, then erase 3 blocks, bit static but its working
  -- Depending if the sudoku needs have an unique solution or not: If it needs to have a unique solution 5 blocks are impossible because 
  -- it is either 3 in a row or the one in the middle and the corners. With 4 it depends on other empty blocks positions. If it does not need to have an unique solution then
  -- 4 and 5 empty blocks are possible.
   
  --Time spend: 6 hours
   
  --The top 3 blocks
  emptyColumns1 = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
  emptyColumns2 = [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
  emptyColumns3 = [(1,7),(1,8),(1,9),(2,7),(2,8),(2,9),(3,7),(3,8),(3,9)]
  emptyColums = [emptyColumns1,emptyColumns2,emptyColumns3]
  
  erasePosition :: Node -> (Row,Column) -> Node
  erasePosition n (r,c) = (s, constraints s) 
    where s = eraseS (fst n) (r,c) 
	
  eraseBlock :: Node -> [(Row,Column)] -> Node
  eraseBlock n [] = n
  eraseBlock n ((r,c):rcs) = eraseBlock (erasePosition n (r,c)) rcs 
  
  assignment4' :: IO ()
  assignment4' =  do [r] <- rsolveNs [emptyN]
                     showNode r
                     let s = (eraseBlock r (concat (emptyColums))) 
                     showNode s  
					 
  --Assignment 5
  --Time spend: 1/2 Hour
  genRandomNRCSudoku :: IO Node
  genRandomNRCSudoku = do [r] <- nrcRsolveNs [emptyN]
                          return r

  nrcRandomS = genRandomNRCSudoku >>= showNode
  
  nrcRsuccNode :: Node -> IO [Node]
  nrcRsuccNode (s,cs) = do xs <- getRandomCnstr cs
                           if null xs 
                             then return []
                             else return 
                               (nrcExtendNode (s,cs\\xs) (head xs))

  nrcRsolveNs :: [Node] -> IO [Node]
  nrcRsolveNs ns = rsearch nrcRsuccNode solved (return ns)
  
  --Helper functions
  nrcExtendNode :: Node -> Constraint -> [Node]
  nrcExtendNode (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           nrcPrune (r,c,v) constraints) | v <- vs ]
		   
  nrcPrune :: (Row,Column,Value) 
        -> [Constraint] -> [Constraint]
  nrcPrune _ [] = []
  nrcPrune (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
    | sameblock (r,c) (x,y) = 
          (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
	| nrcSameblock (r,c) (x,y) = 
          (x,y,zs\\[v]) : nrcPrune (r,c,v) rest
    | otherwise = (x,y,zs) : nrcPrune (r,c,v) rest		   
	
  nrcSameblock :: (Row,Column) -> (Row,Column) -> Bool
  nrcSameblock (r,c) (x,y) = nrcB1 r == nrcB1 x && nrcB1 c == nrcB1 y 
  
  nrcblocks :: [[Int]]
  nrcblocks = [[2..4],[6..8]]
  
  nrcB1 :: Int -> [Int]
  nrcB1 x = concat $ filter (elem x) nrcblocks