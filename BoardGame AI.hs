{-
Names: Devon Puchailo, Molham Kaadan
NSID: drp882, mlk340
STUD.Nums: 11201217, 11160627
CMPT 317 A2
-}
--A data type meant to represent the pieces of this game.
data Piece = E | Q | D | W deriving (Show, Eq)

--A data type that shows the move that will get you from one state to another.
data Move = Do Int Int deriving Eq

--A data type that will represent the full game board.
data Position = Board [Piece] deriving Eq

--A board that will represent the initial state of our game.
emptyBoard :: Position
emptyBoard = Board [E,E,Q,E,E,E,D,D,D,E,E,E,E,E,E,E,E,E,E,E,W,W,W,W,W]

--Terminal states
--P1 Wins if the queen reaches the end or if all the Wights are captured.
winP1 :: Position -> Bool
winP1 (Board list) = list !! 24 == Q || list !! 23 == Q || list !! 22 == Q || list !! 21 == Q || list !! 20 == Q || (W `elem` list) == False

--P2 wins if the queen is captured.
winP2 :: Position -> Bool
winP2 (Board list) = if Q `elem` list then False else True

--Show the board nicely
instance Show Position where
  show (Board [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y]) = "\n" ++ show [a,b,c,d,e] ++ "\n" ++ show [f,g,h,i,j] ++ "\n" ++ show [k,l,m,n,o] ++ "\n" ++ show [p,q,r,s,t] ++ "\n" ++ show [u,v,w,x,y] ++ "\n"
terminateGame :: Position ->  Bool
terminateGame place | winP1 place || winP2 place = True
                    | otherwise = False

--Show moves nicely.
instance Show Move where
 show (Do a b) = show a ++ " to " ++ show b ++ "\n"


--Find all possible moves for d1 using helper functions below.
p1Moves :: Position -> Int -> [(Move, Position)]
p1Moves _ 25 = []
p1Moves (Board list) i | list !! i == D || list !! i == Q = getPosition (Board list) i 1 ++ getPosition (Board list) i 4 ++ getPosition (Board list) i 6 ++ getPosition (Board list) i 5 ++ p1Capture (Board list) i 1 ++ p1Capture (Board list) i 4 ++ p1Capture (Board list) i 5 ++ p1Capture (Board list) i 6 ++ p1Moves (Board list) (i+1)
                       | otherwise = p1Moves (Board list) (i+1)


--Find all the moves for P2 using helper functions below.
p2Moves :: Position -> Int -> [(Move, Position)]
p2Moves _ 25 = []
p2Moves (Board list) i | list !! i == W = getPosition' (Board list) i 1 ++ getPosition' (Board list) i 5 ++ p2Capture (Board list) i 4 ++ p2Capture (Board list) i 6 ++ (p2Moves (Board list) (i+1))
                       | otherwise = p2Moves (Board list) (i+1)

--Swapper that will actually move two pieces on the board
swapper :: Int -> Int -> [a] -> [a]
swapper f s xs = map snd . foldr (\x a ->
        if fst x == f then ys !! s : a
        else if fst x == s then ys !! f : a
        else x : a) [] $ ys
           where ys = zip [0..] xs

--A function that take [(Move, Position)] pairs and provide with a list of possible moves.
fix :: [(a, b)] -> [b]
fix [] = []
fix (x:xs) = [snd x] ++ fix xs

--function that will move D's and Q's into empty squares only. Takes in The current board position, the starting index and the distance to the next index.
--Returns all possible states that aren't captures for the piece we're currently looking at, and the moves to get to said states.
getPosition :: Position -> Int -> Int ->  [(Move, Position)]
getPosition (Board list) start 1 | start - 1 < 0 && start+1 < 25 = if (list !! (start+1)) == E then [(Do start (start+1), Board (swapper start (start+1) list))] else []
                                 | start-1 >= 0 && start+1 > 24 = if (list !! (start-1)) == E then [(Do start (start-1), Board (swapper start (start-1) list))] else []
                                 | start-1 >= 0 && start+1 <= 24 && ((start+1) `mod` 5 == 0) = if (list !! (start-1)) == E then [(Do start (start-1), Board (swapper start (start-1) list))] else []
                                 | start-1 >= 0 && start +1 <= 24 && (start `mod` 5 == 0) = if (list !! (start+1)) == E then [(Do start (start+1), Board (swapper start (start+1) list))] else []
                                 | start-1 >= 0 && start+1 <= 24 = if (list !! (start-1)) == E && (list !! (start+1)) == E then [(Do start (start-1), Board (swapper start (start-1) list))] ++ [(Do start (start+1), Board (swapper start (start+1) list))] else if (list !! (start-1)) == E then [(Do start (start-1), Board (swapper start (start-1) list))] else if (list !! (start+1)) == E then [(Do start (start+1), Board (swapper start (start+1) list))] else []


getPosition (Board list) start end | end == 6 &&  (start `mod` 5 == 0) = if start+end <= 24 && ((list !! (start+end)) == E) then [(Do start (start+end), Board (swapper start (start+end) list))] else []
                                   | end == 4 &&  (start `mod` 5 == 0) = if (start-4 >= 0) && (( list !! (start-end)) == E) then [(Do start (start-end), Board (swapper start (start-end) list ))] else []
                                   | end == 6 && ((start+1) `mod` 5 == 0) = if (start-6) >= 0 && ((list !! (start-end)) == E) then [(Do start (start-end), Board (swapper start (start-end) list ))] else []
                                   | end == 4 && ((start+1) `mod` 5 == 0) = if (start+4) <= 24 && ((list !! (start+end)) == E) then [(Do start (start+end), Board (swapper start (start+end) list ))] else []
                                   | start-end < 0 && start+end > 24 = []
                                   | start-end < 0 && start+end <= 24 = if ((list !! (start+end)) == E) then [(Do start (start+end), Board (swapper start (start+end) list))] else []
                                   | start+end > 24 && start-end >= 0 = if ((list !! (start-end)) == E) then [(Do start (start-end), Board (swapper start (start-end) list))] else []
                                   | start-end >= 0 && start+end <= 24 && ((list !! (start+end)) == E) && ((list !! (start-end)) == E) = [(Do start (start-end), Board (swapper start (start-end) list))] ++ [(Do start (start+end), Board (swapper start (start+end) list))]
                                   | start-end >= 0 && start+end <= 24 && ((list !! (start+end)) == E) = [(Do start (start+end), Board (swapper start (start+end) list))]
                                   | start-end >= 0 && start+end <= 24 && ((list !! (start-end)) == E) = [(Do start (start-end), Board (swapper start (start-end) list))]
                                   | otherwise = []

--function that will move W's into empty squares only. Takes in the index of the piece we're looking at and the distance to the spaces it can move to.
--Returns a list of possible states with the moves to get to said states.
getPosition' :: Position -> Int -> Int ->  [(Move, Position)]
getPosition' (Board list) start 1 | start-1 < 0 && start+1 < 25 = if (list !! (start+1)) == E then [(Do start (start+1), Board (swapper start (start+1) list))] else []
                                  | start-1 >= 0 && start+1 > 24 = if (list !! (start-1)) == E then [(Do start (start-1), Board (swapper start (start-1) list))] else []
                                  | start-1 >= 0 && start+1 <= 24 && ((start+1) `mod` 5 == 0) = if (list !! (start-1)) == E then [(Do start (start-1), Board (swapper start (start-1) list))] else []
                                  | start-1 >= 0 && start +1 <= 24 && (start `mod` 5 == 0) = if (list !! (start+1)) == E then [(Do start (start+1), Board (swapper start (start+1) list))] else []
                                  | start-1 >= 0 && start+1 <= 24 = if (list !! (start-1)) == E && (list !! (start+1)) == E then [(Do start (start-1), Board (swapper start (start-1) list))] ++ [(Do start (start+1), Board (swapper start (start+1) list))] else if (list !! (start-1)) == E then [(Do start (start-1), Board (swapper start (start-1) list))] else if (list !! (start+1)) == E then [(Do start (start+1), Board (swapper start (start+1) list))] else []
getPosition' (Board list) start end | start-end < 0 && start+end > 24 = []
                                    | start-end < 0 && start+end <= 24 = if (list !! (start+end)) == E then [(Do start (start+end), Board (swapper start (start+end) list))] else []
                                    | start+end > 24 && start-end >= 0 = if ((list !! (start-end)) == E ) then [(Do start (start-end), Board (swapper start (start-end) list))] else []
                                    | start-end >= 0 && start+end <= 24 && (list !! (start+end)) == E && (list !! (start-end)) == E = [(Do start (start-end), Board (swapper start (start-end) list))] ++ [(Do start (start+end), Board (swapper start (start+end) list))]
                                    | start-end >= 0 && start+end <= 24 && (list !! (start+end)) == E  = [(Do start (start+end), Board (swapper start (start+end) list))]
                                    | start-end >= 0 && start+end <= 24 && (list !! (start-end)) == E  = [(Do start (start-end), Board (swapper start (start-end) list))]
                                    | otherwise = []

-- Function that will capture elements (with a d or q capturing a w).
--Pre: The position that the board is at, the index of the piece we're looking at, and the indices we want to check.
--Post: None
--Return: a list of [(Move, Position)] pairs that will show all possible states from current state for a piece and the moves of how to get there.
p1Capture :: Position -> Int -> Int -> [(Move, Position)]
p1Capture (Board list) start 1 | start - 1 < 0 && start+1 < 25 = if (list !! (start+1)) == W then [(Do start (start+1), Board (changeElement (swapper start (start+1) list) start))] else []
                               | start-1 >= 0 && start+1 > 24 = if (list !! (start-1)) == W then [(Do start (start-1), Board (changeElement (swapper start (start-1) list) start))] else []
                               | start-1 >= 0 && start+1 <= 24 && ((start+1) `mod` 5 == 0) = if (list !! (start-1)) == W then [(Do start (start-1), Board (changeElement (swapper start (start-1) list) start))] else []
                               | start-1 >= 0 && start +1 <= 24 && (start `mod` 5 == 0) = if (list !! (start+1)) == W then [(Do start (start+1), Board (changeElement (swapper start (start+1) list) start))] else []
                               | start-1 >= 0 && start+1 <= 24 = if (list !! (start-1)) == W && (list !! (start+1)) == W then [(Do start (start-1), Board (changeElement (swapper start (start-1) list) start))] ++ [(Do start (start+1), Board (changeElement (swapper start (start+1) list) start))] else if (list !! (start-1)) == W then [(Do start (start-1), Board (changeElement (swapper start (start-1) list) start))] else if (list !! (start+1)) == W then [(Do start (start+1), Board (changeElement(swapper start (start+1) list) start))] else []

p1Capture (Board list) start end | start `mod` 5 == 0 && end == 6 = if start+6 <= 24 && ((list !! (start+6)) == W) then [(Do start (start+end), Board (changeElement (swapper start (start+end) list ) start))] else []
                                 | (start+1) `mod` 5 == 0 && end == 6 = if start-6 >= 0 && ((list !! (start-6)) == W) then [(Do start (start-end), Board (changeElement (swapper start (start-end) list) start))] else []
                                 | start `mod` 5 == 0 && end == 4 = if start-4 >= 0 && ((list !! (start-4)) == W) then [(Do start (start-end), Board (changeElement (swapper start (start-end) list ) start))] else []
                                 | (start+1) `mod` 5 == 0 && end == 4 = if start+4 <= 24 && ((list !! (start+4)) == W) then [(Do start (start+end), Board (changeElement (swapper start (start+end) list ) start )) ] else []
                                 | start-end < 0 && start+end > 24 = []
                                 | start-end < 0 && start+end <= 24 = if ((list !! (start+end)) == W) then [(Do start (start+end), Board (changeElement (swapper start (start+end) list) start))] else []
                                 | start+end > 24 && start-end >= 0 = if ((list !! (start-end)) == W) then [(Do start (start-end), Board (changeElement (swapper start (start-end) list) start))] else []
                                 | start-end >= 0 && start+end <= 24 && ((list !! (start+end)) == W) && ((list !! (start-end)) == W) = [(Do start (start-end), Board (changeElement (swapper start (start-end) list) start))] ++ [(Do start (start+end), Board (changeElement (swapper start (start+end) list) start))]
                                 | start-end >= 0 && start+end <= 24 && ((list !! (start+end)) == W) = [(Do start (start+end), Board (changeElement (swapper start (start+end) list) start))]
                                 | start-end >= 0 && start+end <= 24 && ((list !! (start-end)) == W) = [(Do start (start-end), Board (changeElement (swapper start (start-end) list) start))]
                                 | otherwise = []

--Purpose: To allow wights to capture Q's and D's.
--Pre: The position of the board, the index of a piece we're looking at, and the distance we are from the index(es) that we want to check.
--Post: None
--Return: A list of move, position pairs that will show us possible states for the given piece and the moves on how to get to said space from our current space.
p2Capture :: Position -> Int -> Int -> [(Move, Position)]
p2Capture (Board list) start end | end == 4 && start+1 `mod` 5 == 0  = if start+4 <= 24 && (list !! (start+end)) == D || (list !! (start+end) == Q) then [(Do start (start+end), Board (changeElement (swapper start (start+end) list) start))] else []
                                 | end == 4 && start `mod` 5 == 0 = if start-4 >= 0 && (list !! (start-end)) == D || (list !! (start-end) == Q) then [(Do start (start-end), Board (changeElement (swapper start (start-end) list) start))] else []
                                 | end == 6 && (start `mod` 5 == 0) = if start+6 <= 24 && ((list !! (start+end)) == D || (list !! (start+end) == Q)) then [(Do start (start+end), Board (changeElement (swapper start (start+end) list) start))] else []
                                 | end == 6 && start+1 `mod` 5 == 0  = if start-6 >= 0 && (list !! (start-end)) == D || (list !! (start-end) == Q) then [(Do start (start-end), Board (changeElement (swapper start (start-end) list ) start))] else []
                                 | start-end < 0 && start+end <= 24 = if (list !! (start+end)) == D || (list !! (start+end) == Q) then [(Do start (start+end), Board (changeElement (swapper start (start+end) list) start))] else []
                                 | start-end >= 0 && start+end > 24 = if (list !! (start-end)) == D || (list !! (start-end) == Q) then [(Do start (start-end), Board (changeElement (swapper start (start-end) list) start))] else []
                                 | start-end >= 0 && start+end <= 24 && ((list !! (start+end)) == D || (list !! (start+end)) == Q) && ((list !! (start-end)) == D || (list !! (start-end)) == Q) = [(Do start (start-end), Board (changeElement (swapper start (start-end) list) start))] ++ [(Do start (start+end), Board (changeElement (swapper start (start+end) list) start))]
                                 | start-end >= 0 && start+end <= 24 && ((list !! (start+end)) == D || (list !! (start+end)) == Q) = [(Do start (start+end), Board (changeElement (swapper start (start+end) list) start))]
                                 | start-end >= 0 && start+end <= 24 && ((list !! (start-end)) == D || (list !! (start-end)) == Q) = [(Do start (start-end), Board (changeElement (swapper start (start-end) list) start))]
                                 | otherwise = []

--Purpose: A helper function meant to aid in capturing pieces.
--Pre:Takes in the list of pieces currently associated with the position the board is in on and the integer we want to change to empty.
--Post: None
--Returns: A list of pieces that will have one more empty square and one less of the piece we captured.
changeElement :: [Piece] -> Int -> [Piece]
changeElement [] _ = []
changeElement (x:xs) 0 = [E] ++ xs
changeElement (x:xs) n = [x] ++ changeElement xs (n-1)


--Purpose: To provide the list of pieces associated with the position of the game board.
--Pre: The position of the game board
--Post: None
--Return: The list of pieces associated with the gameboards state.
board2list :: Position -> [Piece]
board2list (Board list) = list

--Purpose: To provide us a goodness-of-state evaluation for a gameboard.
--Pre: A list of pieces associated with the gameboards state, an int that will increment through the indices of the gameboard, and the heuristic value of the placement of the queen.
--Post: None
--Return: An integer symbolizing the goodness-of-state of the gameboard.
heuristicEval :: [Piece] -> Int -> Int -> Int
heuristicEval [] _ value = value
heuristicEval (x:xs) n val | x == E || x == Q = heuristicEval xs (n+1) val
                           | x == W = (-10) + (heuristicEval xs (n+1)) val
                           | x == D = 5 + (heuristicEval xs (n+1)) val

--Purpose: To get the value of the queens position
--Pre: The list of pieces associated iwth the gameboards current position.
--Post: None
--Return: An integer value symbolizing the value of the queen's position.
getQueenPlaceValue :: [Piece] -> Int
getQueenPlaceValue list | Q `elem` (take 5 list) = 0
                        | Q `elem` (take 5 (drop 5 list)) = 10
                        | Q `elem` (take 5 (drop 10 list)) = 20
                        | Q `elem` (take 5 (drop 15 list)) = 30

--Purpose: To calculate the minimax value at our current position.
--Pre: How far we are from the allowed bottom of search, a boolean that will represent the current player. True for Q's team, False for W's team. The position of the gameboard.
--Post: None
--Return: An integer value representing the minimax value for the current position.
miniMax :: Int -> Bool -> Position -> Int
miniMax 1 _ (Board list) | winP1 (Board list) = 100
                         | winP2 (Board list) = -100
                         | otherwise = heuristicEval list 0 (getQueenPlaceValue list)
miniMax n True pos = maximum (map (miniMax (n-1) False) (fix (p1Moves pos 0)))
miniMax n False pos = minimum (map (miniMax (n-1) True) (fix (p2Moves pos 0)))

--Purpose: get all minimax values for subtrees
--Pre: An integer value symbolizing maximum depth allowed. A boolean representing the current player. True for Q's team, False for W's team. The position of the current game board.
--Post: None
--Return: The minimax values, in order, of each of the current positions subtrees.
miniMaxAll :: Int -> Bool -> Position -> [Int]
miniMaxAll n True pos = map (miniMax (n-1) False) (fix (p1Moves pos 0))
miniMaxAll n False pos = map (miniMax (n-1) True ) (fix (p2Moves pos 0))

getMiniMax :: Int -> Bool -> Position -> Int
getMiniMax n bool pos = miniMax n bool pos

--Purpose: To iteratively do a miniMax depth first search on the search tree.
--Pre: An integer symbolizing max depth allowed, the current depth, a boolean representing the current player. True for Q's team, False for W's team. The list of seen positionsThe position of the game board
--Post: None
--Return: The best possible position for the player to make.
iterative :: Int -> Int -> Bool -> [Position] -> Position -> Position
iterative n cur bool seen pos | n == cur = pickMoveIterative (matchToPos pos bool n) (miniMax cur bool pos) seen
                               | miniMax cur True pos == 100 = pickMoveIterative  (matchToPos pos bool cur) (miniMax cur bool pos) seen
                               | miniMax cur False pos == (-100) = pickMoveIterative (matchToPos pos bool cur) (miniMax cur bool pos) seen
                               | otherwise = iterative n (cur+1) bool seen pos


--Purpose: To match positions to their minimax values
--Pre: The position of the gameboard, a boolean representing the current player (True for Q's team, False for W's Team). n as the maximum depth of the iterative search.
--Post: None
--Return: A list of integer, position pairs that will represent positions maxed to their minimax values.
matchToPos :: Position -> Bool -> Int -> [(Int, Position)]
matchToPos pos player n | player = zip (miniMaxAll n player pos) (fix (p1Moves pos 0))
                        | otherwise = zip (miniMaxAll n player pos) (fix (p2Moves pos 0))

-- Purpose: To pick one move from the integer, position pairs provided to us from matchToPos
--Pre: The integer pairs provided to us from matchToPos, the integer value symbolizing the minimax value we're looking for (given by  function miniMax), and the list of seen positions.
--Post: None
--Return: The best position possible for this player.
pickMove :: [(Int, Position)] -> Int -> [Position] -> Position
pickMove [] _ _ = Board []
pickMove (x:xs) (-100) seen | (-100) == (fst x) = snd x
                            | otherwise = pickMove xs (-100) seen

pickMove (x:xs) n seen | n == (fst x) && (((snd x) `elem` seen) == False)  = snd x
                       | otherwise = pickMove xs n seen


--Purpose: To do an iterative deepning search on the search tree to get the best position for a move.
--Pre: The int position pairs provided from matchToPos, the miniMax value from the function miniMax, the list of seen positions.
--Post: None
--Returns: The best position possible for this player at this time.
pickMoveIterative :: [(Int, Position)] -> Int -> [Position] -> Position
pickMoveIterative list (-100) seen | pickMove list (-100) seen /= Board [] = pickMove list (-100) seen
                                   | otherwise = error "No possible moves!"
pickMoveIterative list n seen | pickMove list n seen /= Board [] = pickMove list n seen
                              | otherwise = pickMoveIterative list (n-1) seen


--doMove and possibleMoves are for human interaction only.
--Purpose: Provide a list of possible moves to match to positions.
--Pre: The list of integer, positions pairs from function match to position
--Post: None
--Return: A list of moves we can do.
possibleMoves :: [(Move, Position)] -> [Move]
possibleMoves []  = []
possibleMoves (x:xs) = [fst x] ++ possibleMoves xs

--Purpose: To do a move (for a human)
--Pre: The list of move, position pairs from p1Moves or p2Moves. The move we wish to do.
--Post: None
--Returns: The new position of the game board after we perform our move.
doMove :: [(Move, Position)] -> Move -> Position
doMove [] _ = Board []
doMove (x:xs) theMove | (fst x) == theMove = snd x
                      | otherwise = doMove xs theMove

--Computers play eachother default values plaAI emptyBoard n 0 False []
playAI position int True seen | winP1 position = do putStrLn ("Player 1 wins! Thank you for playing!")
                              | winP2 position = do putStrLn ("Player 2 wins! Thank you for playing!")
                              | otherwise = let newMove = iterative int 1 True seen position
                                             in do putStrLn ("Player 1's Move!")
                                                   putStrLn (show newMove)
                                                   do playAI newMove int False (seen ++ [newMove])
playAI position int False seen | winP1 position = do putStrLn ("Player 1 wins! Thank you for playing!")
                               | winP2 position = do putStrLn ("Plaer 2 wins! Thank you for playing!")
                               | otherwise = let newMove = iterative int 1 False seen position
                                             in do putStrLn ("Player 2's Move!")
                                                   putStrLn (show newMove)
                                                   do playAI newMove int True (seen ++ [newMove])


--Purpose: To make an AI play another AI
play = do putStrLn (show emptyBoard)
          do playAI emptyBoard 5 False []

--Purpose: To play against an AI.
main = do
       putStrLn ("While playing this game, you will be continuously asked to select indexes. Indexes can be selected as follows:")
       putStrLn("[ 0, 1, 2, 3, 4]")
       putStrLn("[ 5, 6, 7, 8, 9]")
       putStrLn("[10,11,12,13,14]")
       putStrLn("[15,16,17,18,19]")
       putStrLn("[20,21,22,23,25]")
       putStrLn (show emptyBoard)
       putStrLn ("Please input a 1 if you want to play as the Queen's team, or 2 if you want to play as the Wights team!")
       team <- getLine
       case team of "1" -> do aITurnWight emptyBoard []
                    "2" -> do humanTurnWight emptyBoard []

aITurnWight pos seen | winP1 pos = putStrLn ("Player 1 wins! Thank you for playing!")
                     | winP2 pos = putStrLn ("Player 2 wins! Thank you for playing!")
                     | otherwise = do
                                   let theMove = iterative 5 1 False seen pos
                                    in do putStrLn (show pos)
                                          putStrLn (show theMove)
                                          do humanTurnQueen theMove (seen ++ [theMove])

aITurnQueen pos seen | winP1 pos = putStrLn ("Player 1 wins! Thank you for playing!")
                     | winP2 pos = putStrLn ("Player 2 wins! Thank you for playing!")
                     | otherwise =  do
                                    let theMove = iterative 5 1 True seen pos
                                     in do putStrLn (show pos)
                                           putStrLn (show theMove)
                                           do humanTurnWight theMove (seen ++ [theMove])

humanTurnWight (Board list) seen | winP1 (Board list) = putStrLn ("Player 1 wins! Thank you for playing!")
                                 | winP2 (Board list) = putStrLn ("Player 2 wins! Thank you for playing!")
                                 | otherwise = do
                                               putStrLn ("Please input the index of the piece you want to move!")
                                               piece <- readLn
                                               let thePiece = (piece :: Int)
                                               if (thePiece >= 0 && thePiece <= 24 && ((list !! thePiece) == W)) then do
                                                                                                                      putStrLn ("Please enter the index to which you want to move!")
                                                                                                                      move <- readLn
                                                                                                                      let theMove = (move :: Int)
                                                                                                                      if (theMove >= 0 && theMove <= 24) && ((Do thePiece theMove) `elem` possibleMoves (p2Moves (Board list) 0)) then aITurnQueen (doMove  (p2Moves (Board list) 0) (Do thePiece theMove)) (seen ++ [doMove (p2Moves (Board list) 0) (Do thePiece theMove) ])
                                                                                                                      else do putStrLn ("Did something wrong! Try again!")
                                                                                                                              do humanTurnWight (Board list) seen
                                               else do humanTurnWight (Board list) seen

humanTurnQueen (Board list) seen | winP1 (Board list) = putStrLn ("Player 1 wins! Thank you for playing!")
                                 | winP2 (Board list) = putStrLn ("Player 2 wins! Thank you for playing!")
                                 | otherwise =  do
                                                putStrLn ("Please input the index of the piece you want to move!")
                                                piece <- readLn
                                                let thePiece = (piece :: Int)
                                                if (thePiece >= 0 && thePiece <= 24) && ((list !! thePiece) == Q || (list !! thePiece) == D) then do
                                                                                                                                                  putStrLn ("Please enter the index to which you want to move!")
                                                                                                                                                  move <- readLn
                                                                                                                                                  let theMove = (move :: Int)
                                                                                                                                                  if (theMove >= 0 && theMove <= 24) && ((Do thePiece theMove) `elem` possibleMoves (p1Moves (Board list) 0)) then aITurnWight (doMove (p1Moves (Board list) 0) (Do thePiece theMove)) (seen ++ [doMove (p2Moves (Board list) 0) (Do thePiece theMove) ])
                                                                                                                                                  else do putStrLn ("Did something wrong! Try again!")
                                                                                                                                                          do humanTurnQueen (Board list) seen
                                                else do humanTurnQueen (Board list) seen
