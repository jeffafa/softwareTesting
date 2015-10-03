  module Week5
  
  where 
  
  import Data.List
  import System.Random
  import Lecture5
  import Data.Char
  
  type Position = (Row,Column)
  type Constrnt = [[Position]]
    
  --assignment 1 -- 3 hours
  nrcblocks :: [[Int]]
  nrcblocks = [[2..4],[6..8]]
  
  nrcB1 :: Int -> [Int]
  nrcB1 x = concat $ filter (elem x) nrcblocks
  
  nrcSubGrid :: Sudoku -> (Row,Column) -> [Value]
  nrcSubGrid s (r,c) = [ s (r',c') | r' <- nrcB1 r, c' <- nrcB1 c ]
  
  nrcFreeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
  nrcFreeInSubgrid s (r,c) = freeInSeq (nrcSubGrid s (r,c))
  
  nrcFreeAtPos :: Sudoku -> (Row,Column) -> [Value]
  nrcFreeAtPos s (r,c) = 
    (freeInRow s r) 
     `intersect` (freeInColumn s c) 
     `intersect` (freeInSubgrid s (r,c))
	 `intersect` (nrcFreeInSubgrid s (r,c))
  
  nrcSubgridInjective :: Sudoku -> (Row,Column) -> Bool
  nrcSubgridInjective s (r,c) = injective vs where 
     vs = filter (/= 0) (nrcFreeInSubgrid s (r,c))
  
  nrcConsistent :: Sudoku -> Bool
  nrcConsistent s = and $
                 [ rowInjective s r |  r <- positions ]
                  ++
                 [ colInjective s c |  c <- positions ]
                  ++
                 [ subgridInjective s (r,c) | 
                      r <- [1,4,7], c <- [1,4,7]]
  
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
  
  nrcSolveNs :: [Node] -> [Node]
  nrcSolveNs = search nrcSuccNode solved 
  
  nrcSuccNode :: Node -> [Node]
  nrcSuccNode (s,[]) = []
  nrcSuccNode (s,p:ps) = nrcExtendNode (s,ps) p 

  nrcSolveAndShow :: Grid -> IO[()]
  nrcSolveAndShow gr = nrcSolveShowNs (initNode gr)
  
  nrcSolveShowNs :: [Node] -> IO[()]
  nrcSolveShowNs = sequence . fmap showNode . nrcSolveNs
  
  nrcExtendNode :: Node -> Constraint -> [Node]
  nrcExtendNode (s,constraints) (r,c,vs) = 
     [(extend s ((r,c),v),
       sortBy length3rd $ 
           nrcPrune (r,c,v) constraints) | v <- vs ]

--Assignment 2 -- 4 hours

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

  
  --QuickCheck
  -- I just couldn't get it to work with quickcheck. 
  
  --genSet :: IO (Sudoku)
  --genSet = do
--		x <- genRandomSudoku
--		return (x)
		
--QuickCheck part--
  --prop_Isminimal :: Sudoku -> Bool
  --prop_Isminimal x = isMinimal (sub2Grid x)

--newtype Sudoku' = Sudoku' deriving (Sudoku)

 -- instance Sudoku where
 --   arbitrary = do
 --               y <- genRandomSudoku
 --               return y
 
 
				
--positiveIntGen :: Gen (Positive Int)
--positiveIntGen = arbitrary

--assignment 5   		
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
				
  --Just some examples
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
