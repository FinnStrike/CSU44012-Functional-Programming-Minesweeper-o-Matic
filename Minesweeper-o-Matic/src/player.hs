module Player (module Player) where

import Control.Monad

import Graphics.UI.Threepenny.Core hiding ((<|>), grid, style, row)

import Data.IORef
import Data.Maybe
import qualified Data.List as L
import Minesweeper

-- Attempt to play the best move available
-- Despite the branding it does NOT use AI ;)
playMove :: IORef [[Square]] -> IORef Bool -> UI ()
playMove squaresRef gameState = do
    squares <- liftIO $ readIORef squaresRef
    active <- liftIO $ readIORef gameState
    when active $ do
        -- Attempt to find an unambiguously safe square to reveal
        case findSafeReveal squares of
            Just (r, c) -> do
                -- Reveal the safe square
                let square = squares !! r !! c
                let newSquare = reveal square
                liftIO $ updateSquareInGrid squaresRef r c newSquare
                when (isEmpty newSquare) $ revealNeighbours squaresRef r c
                -- Log the move
                liftIO $ putStrLn $ "Revealed square at (" ++ show r ++ ", " ++ show c ++ ")."
            Nothing -> do
                -- Attempt to find a definite mine to flag
                case findFlagMove squares of
                    Just (r, c) -> do
                        -- Flag the mine
                        let square = squares !! r !! c
                        let newSquare = flag square
                        liftIO $ updateSquareInGrid squaresRef r c newSquare
                        -- Log the move
                        liftIO $ putStrLn $ "Flagged square at (" ++ show r ++ ", " ++ show c ++ ")."
                    Nothing -> do
                        -- Attempt to find a definite mine to flag using the 1-2-X pattern
                        case find12XFlagMove squares of
                            Just (r, c) -> do
                                -- Flag the mine
                                let square = squares !! r !! c
                                let newSquare = flag square
                                liftIO $ updateSquareInGrid squaresRef r c newSquare
                                -- Log the move
                                liftIO $ putStrLn $ "Flagged square at (" ++ show r ++ ", " ++ show c ++ ")."
                            Nothing -> do
                                -- If all else fails, attempt to find the least dangerous square to reveal
                                case findLeastDangerousReveal squares of
                                    Just (r, c) -> do
                                        -- Reveal the returned square and pray it's safe
                                        let square = squares !! r !! c
                                        let newSquare = reveal square
                                        liftIO $ updateSquareInGrid squaresRef r c newSquare
                                        when (isEmpty newSquare) $ revealNeighbours squaresRef r c
                                        -- Log the move
                                        liftIO $ putStrLn $ "Revealed square at (" ++ show r ++ ", " ++ show c ++ ")."
                                    Nothing -> do
                                        -- Log that no move was found (this case should no longer occur)
                                        liftIO $ putStrLn "No safe moves available."
                                        return ()

-- Attempt to find an unambiguously safe square to reveal
findSafeReveal :: [[Square]] -> Maybe (Int, Int)
findSafeReveal squares = 
    let size = length squares
        coords = [(r, c) | r <- [0..size-1], c <- [0..size-1]]
        safeMoves = [ (nr, nc)
                    | (r, c) <- coords
                    -- Check every revealed square
                    , let square = squares !! r !! c
                    , isRevealed square
                    -- Get the hidden neighbours of each square
                    , let neighbours = getNeighbours squares r c
                    , let hiddenNeighbours = [(nr, nc) | (nr, nc) <- neighbourCoords r c size, isClearAndHidden (squares !! nr !! nc)]
                    -- Check if number of flagged neighbours = clue
                    , let flaggedCount = length $ filter isFlagged neighbours
                    , flaggedCount == countMines square
                    -- If so we can return one of the other neighbours to be revealed
                    , not (null hiddenNeighbours)
                    , let (nr, nc) = head hiddenNeighbours ]
    -- Return the first match if it exists
    in listToMaybe safeMoves

-- Attempt to find a definite mine that can be flagged
findFlagMove :: [[Square]] -> Maybe (Int, Int)
findFlagMove squares = 
    let size = length squares
        coords = [(r, c) | r <- [0..size-1], c <- [0..size-1]]
        flagMoves = [ (nr, nc)
                    | (r, c) <- coords
                    -- Check every revealed square
                    , let square = squares !! r !! c
                    , isRevealed square
                    -- Get the unflagged hidden neighbours of each square
                    , let clearhiddenNeighbours = [(nr, nc) | (nr, nc) <- neighbourCoords r c size, isClearAndHidden (squares !! nr !! nc)]
                    , let neighbours = getNeighbours squares r c
                    -- Check if number of hidden neighbours = clue - flagged neighbours
                    , let flaggedCount = length $ filter isFlagged neighbours
                    , let clue = countMines square
                    , length clearhiddenNeighbours == clue - flaggedCount
                    -- If so we can return one of the neighbours to be flagged
                    , not (null clearhiddenNeighbours)
                    , let (nr, nc) = head clearhiddenNeighbours ]
    -- Return the first match if it exists
    in listToMaybe flagMoves

-- Attempt to flag a mine using the 1-2-X pattern
find12XFlagMove :: [[Square]] -> Maybe (Int, Int)
find12XFlagMove squares = 
    let size = length squares
        coords = [(r, c) | r <- [0..size-1], c <- [0..size-1]]
        -- Check for the 1-2-X pattern in every position and direction
        flagMoves12X = [ (nr, nc)
                       | (r, c) <- coords
                       , Just (nr, nc) <- 
                            [ get12XRow squares r c (-1) 1
                            , get12XRow squares r c (-1) (-1)
                            , get12XRow squares r c 1 1
                            , get12XRow squares r c 1 (-1) 
                            , get12XCol squares r c (-1) 1
                            , get12XCol squares r c (-1) (-1)
                            , get12XCol squares r c 1 1
                            , get12XCol squares r c 1 (-1) ]]
        -- Return the first match if it exists
    in listToMaybe (flagMoves12X)

-- Check if a grid section satisfies the criteria for 1-2-X in rows
--   We pass in the coordinates of the 1 Square
--   We pass in a parameter p to determine which position we search
--   If p = -1 we search the row above, if p = +1 we search the row below
--   We pass in a parameter d to determine which direction we search (right/left)
--   If d = +1 we search for 1-2-X, if d = -1 we search for X-2-1
-- Returns the coordinates of the Square to Flag if pattern found
get12XRow :: [[Square]] -> Int -> Int -> Int -> Int -> Maybe (Int, Int)
get12XRow squares i j p d = 
    let size = length squares
        -- Calculate the coordinates of adjacent squares based on the direction and position
        square = squares !! i !! j
        n1 = squares !! i !! (j+(d*1))
        n2 = squares !! i !! (j+(d*2))
        -- Calculate the "search" row (above or below depending on p)
        s1 = squares !! (i+(p*1)) !! j
        s2 = squares !! (i+(p*1)) !! (j+(d*1))
        s3 = squares !! (i+(p*1)) !! (j+(d*2))
        -- Check the "discard" row (above or below depending on p)
        d1Safe = safeSquare squares size (i-(p*1)) j
        d2Safe = safeSquare squares size (i-(p*1)) (j+(d*1))
        d3Safe = safeSquare squares size (i-(p*1)) (j+(d*2))
        -- Helper values for checking bounds
        oP1 = (1-p) `div` 2        -- 1 when p is -1, 0 when p is 1
        oP2 = (1-(p*(-1))) `div` 2 -- 0 when p is -1, 1 when p is 1
        oD1 = 1 - d                -- 2 when d is -1, 0 when d is 1
        oD2 = 1 - (d*(-1))         -- 0 when d is -1, 2 when d is 1
    -- Ensure all 1-2-X criteria are satisfied
    in if isRevealed square
          && (p == 1 || p == -1)
          && (d == 1 || d == -1)
          && i-oP1 >= 0 && i+oP2 < size
          && j-oD1 >= 0 && j+oD2 < size
          && isRevealed n1 && isRevealed n2
          && isHidden s1 && isHidden s2 && isHidden s3
          && d1Safe && d2Safe && d3Safe
          && hasNMines square 1
          && hasNMines n1 2
          && not (isFlagged (squares !! (i+(p*1)) !! (j+(d*2))))
    -- If valid pattern has been found, return the coordinates to Flag
       then Just (i+(p*1), j+(d*2))
       else Nothing

-- Check if a grid section satisfies the criteria for 1-2-X in columns
--   We pass in the coordinates of the 1 Square
--   We pass in a parameter p to determine which position we search
--   If p = -1 we search the left column, if p = +1 we search the right column
--   We pass in a parameter d to determine which direction we search (down/up)
--   If d = +1 we search for 1-2-X, if d = -1 we search for X-2-1
-- Returns the coordinates of the Square to Flag if pattern found
get12XCol :: [[Square]] -> Int -> Int -> Int -> Int -> Maybe (Int, Int)
get12XCol squares i j p d = 
    let size = length squares
        -- Calculate the coordinates of adjacent squares based on the direction and position
        square = squares !! i !! j
        n1 = squares !! (i+(d*1)) !! j
        n2 = squares !! (i+(d*2)) !! j
        -- Calculate the "search" column (left or right depending on p)
        s1 = squares !! i         !! (j+(p*1))
        s2 = squares !! (i+(d*1)) !! (j+(p*1))
        s3 = squares !! (i+(d*2)) !! (j+(p*1))
        -- Check the "discard" column (left or right depending on p)
        d1Safe = safeSquare squares size i         (j-(p*1))
        d2Safe = safeSquare squares size (i+(d*1)) (j-(p*1))
        d3Safe = safeSquare squares size (i+(d*2)) (j-(p*1))
        -- Helper values for checking bounds
        oP1 = (1-p) `div` 2        -- 1 when p is -1, 0 when p is 1
        oP2 = (1-(p*(-1))) `div` 2 -- 0 when p is -1, 1 when p is 1
        oD1 = 1 - d                -- 2 when d is -1, 0 when d is 1
        oD2 = 1 - (d*(-1))         -- 0 when d is -1, 2 when d is 1
    -- Ensure all 1-2-X criteria are satisfied
    in if isRevealed square
          && (p == 1 || p == -1)
          && (d == 1 || d == -1)
          && i-oD1 >= 0 && i+oD2 < size
          && j-oP1 >= 0 && j+oP2 < size
          && isRevealed n1 && isRevealed n2
          && isHidden s1 && isHidden s2 && isHidden s3
          && d1Safe && d2Safe && d3Safe
          && hasNMines square 1
          && hasNMines n1 2
          && not (isFlagged (squares !! (i+(d*2)) !! (j+(p*1))))
    -- If valid pattern has been found, return the coordinates to Flag
       then Just (i+(d*2), j+(p*1))
       else Nothing

-- Check if a square is either Revealed or Out of Bounds (used in 1-2-X check)
safeSquare :: [[Square]] -> Int -> Int -> Int -> Bool
safeSquare squares size i j 
    | i >= 0 && i < size && j >= 0 && j < size = isRevealed (squares !! i !! j)
    | otherwise = True

-- Attempt to find the least dangerous reveal available
--   This method involves computing local probabilities for each hidden square
--   Squares with the lowest probability of being a mine will be selected
-- This is NOT the most optimal algorithm for computing probabilities,
--   but in general it serves as a reasonably decent fallback 
--   if no better move can be found
findLeastDangerousReveal :: [[Square]] -> Maybe (Int, Int)
findLeastDangerousReveal squares =
    let probabilities = calculateProbabilities squares
        size = length probabilities
        coords = [(r, c) | r <- [0..size-1], c <- [0..length (head probabilities) - 1]]
        hiddenSquares = [ (r, c)
                        | (r, c) <- coords
                        , let square = squares !! r !! c
                        , not (isRevealed square)
                        , not (isFlagged square) ]
        -- Calculate probabilities for all hidden squares
        probabilitiesForHidden = [ ((r, c), probabilities !! r !! c)
                                  | (r, c) <- hiddenSquares ]
        -- Sort by lowest probability and return the safest move
        safestMove = listToMaybe $ L.sortOn snd probabilitiesForHidden
    in fmap fst safestMove

-- Calculate the probability map for the grid
calculateProbabilities :: [[Square]] -> [[Double]]
calculateProbabilities squares = 
    let size = length squares
        coords = [(r, c) | r <- [0..size-1], c <- [0..length (head squares) - 1]]
        neighbourProbs = concatMap (analyzeSquare squares) coords
        rawProbabilities = aggregateProbabilities size neighbourProbs
    in assignDefaultProbabilities squares rawProbabilities

-- Analyze a single square for its contribution to hidden neighbours
analyzeSquare :: [[Square]] -> (Int, Int) -> [((Int, Int), Double)]
analyzeSquare squares (r, c) = 
    let square = squares !! r !! c
        size = length squares
        neighbours = neighbourCoords r c size
        revealedNeighbours = [sq | (nr, nc) <- neighbours, let sq = squares !! nr !! nc, isRevealed sq]
        hiddenNeighbours = [(nr, nc) | (nr, nc) <- neighbours, isHidden (squares !! nr !! nc)]
    in if isRevealed square && not (isMine square) && not (null revealedNeighbours) && not (null hiddenNeighbours)
       then
            -- Probability of square being a mine = neighbour clue / hidden neighbours
            let n = getMines square
                prob = fromIntegral n / fromIntegral (length hiddenNeighbours)
            in [ (pos, prob) | pos <- hiddenNeighbours ]
       else []

-- Aggregate the probabilities for each square
aggregateProbabilities :: Int -> [((Int, Int), Double)] -> [[Double]]
aggregateProbabilities size neighbourProbs = 
    let initialMap = replicate size (replicate size 0.0)
        groupedProbs = foldl (\acc (pos, prob) -> addProbability acc pos prob) initialMap neighbourProbs
    in averageProbabilities groupedProbs

-- Assign a default probability of 1 to squares with no revealed neighbours
--   This is because we have no information about these squares so we assign
--   them highest probabilities (so they'll be considered as highly dangerous)
assignDefaultProbabilities :: [[Square]] -> [[Double]] -> [[Double]]
assignDefaultProbabilities squares probMap = 
    let size = length squares
        coords = [(r, c) | r <- [0..size-1], c <- [0..length (head squares) - 1]]
        updatedMap = foldl (\acc (r, c) -> 
                    if norevealedNeighbours squares r c
                    then updateProbability acc (r, c) 1.0
                    else acc) probMap coords
    in updatedMap

-- Check if a square has no revealed neighbours
norevealedNeighbours :: [[Square]] -> Int -> Int -> Bool
norevealedNeighbours squares r c = 
    let size = length squares
        neighbours = neighbourCoords r c size
    in null [sq | (nr, nc) <- neighbours, let sq = squares !! nr !! nc, isRevealed sq]

-- Add a probability to a specific grid position
addProbability :: [[Double]] -> (Int, Int) -> Double -> [[Double]]
addProbability probMap (r, c) prob = 
    let (beforeRow, rowAfter) = splitAt r probMap
    in case rowAfter of
        [] -> probMap  -- If the row is empty, just return the original grid
        (row:afterRow) ->
            case splitAt c row of
                (beforeCol, val:afterCol) -> 
                    beforeRow ++ [beforeCol ++ [val + prob] ++ afterCol] ++ afterRow
                _ -> probMap  -- If the column is out of range, return the original grid

-- Update a specific grid position with a new probability
updateProbability :: [[Double]] -> (Int, Int) -> Double -> [[Double]]
updateProbability probMap (r, c) prob = 
    let (beforeRow, rowAfter) = splitAt r probMap
    in case rowAfter of
        [] -> probMap  -- If the row is empty, just return the original grid
        (row:afterRow) ->
            case splitAt c row of
                (beforeCol, _:afterCol) -> 
                    beforeRow ++ [beforeCol ++ [prob] ++ afterCol] ++ afterRow
                _ -> probMap  -- If the column is out of range, return the original grid

-- Average the probabilities for squares with multiple contributions
averageProbabilities :: [[Double]] -> [[Double]]
averageProbabilities probMap = 
    map (map (\r -> if r > 0 then r else 0)) probMap

-- Check if a Square is Clear and Hidden
isClearAndHidden :: Square -> Bool
isClearAndHidden (Clear (Hidden _)) = True
isClearAndHidden _ = False

-- Get the neighbouring Squares of a given Square
getNeighbours :: [[Square]] -> Int -> Int -> [Square]
getNeighbours squares r c =
    [ squares !! nr !! nc
    | dr <- [-1..1], dc <- [-1..1], let nr = r + dr, let nc = c + dc
    , nr >= 0, nc >= 0, nr < length squares, nc < length (head squares)
    , (dr, dc) /= (0, 0) ]

-- Get the neighbouring coordinates of a given position
neighbourCoords :: Int -> Int -> Int -> [(Int, Int)]
neighbourCoords r c size =
    [ (nr, nc)
    | dr <- [-1..1], dc <- [-1..1]
    , let nr = r + dr, let nc = c + dc
    , nr >= 0, nc >= 0, nr < size, nc < size
    , (dr, dc) /= (0, 0) ]

-- Get the number of neighbouring mines of a given Square (i.e. the clue)
countMines :: Square -> Int
countMines (Clear (Revealed (Empty n))) = n
countMines _ = 0