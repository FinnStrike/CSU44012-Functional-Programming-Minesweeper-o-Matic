module Player (module Player) where

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad

import Graphics.UI.Threepenny.Core hiding ((<|>), grid, style, row)

import Data.IORef
import Data.Maybe
import Data.List
import Minesweeper

-- Attempt to play the best move available
playMove :: IORef [[Square]] -> IORef Bool -> UI ()
playMove squaresRef gameState = do
    squares <- liftIO $ readIORef squaresRef
    active <- liftIO $ readIORef gameState
    when active $ do
        -- Attempt to find a safe reveal move
        case findSafeReveal squares of
            Just (x, y) -> do
                -- Reveal the safe square
                let square = squares !! x !! y
                let newSquare = reveal square
                liftIO $ updateSquareInGrid squaresRef x y newSquare
                when (isEmpty newSquare) $ revealNeighbours squaresRef x y
                -- Log the move
                liftIO $ putStrLn $ "Revealed square at (" ++ show x ++ ", " ++ show y ++ ")."
            Nothing -> do
                -- Attempt to find a flag move
                case findFlagMove squares of
                    Just (x, y) -> do
                        -- Flag the mine
                        let square = squares !! x !! y
                        let newSquare = flag square
                        liftIO $ updateSquareInGrid squaresRef x y newSquare
                        -- Log the move
                        liftIO $ putStrLn $ "Flagged square at (" ++ show x ++ ", " ++ show y ++ ")."
                    Nothing -> do
                        -- Attempt to find a case of 1-2-X to flag
                        case find12XFlagMove squares of
                            Just (x, y) -> do
                                -- Flag the mine
                                let square = squares !! x !! y
                                let newSquare = flag square
                                liftIO $ updateSquareInGrid squaresRef x y newSquare
                                -- Log the move
                                liftIO $ putStrLn $ "Flagged square at (" ++ show x ++ ", " ++ show y ++ ")."
                            Nothing -> do
                                -- Log that no move was found
                                liftIO $ putStrLn "No safe moves available."
                                return ()

-- Attempt to find an unambiguously safe move
findSafeReveal :: [[Square]] -> Maybe (Int, Int)
findSafeReveal squares = 
    let size = length squares
        coords = [(x, y) | x <- [0..size-1], y <- [0..size-1]]
        safeMoves = [ (nx, ny)
                    | (x, y) <- coords
                    , let square = squares !! x !! y
                    , isRevealed square
                    , let neighbours = getNeighbours squares x y
                    , let flaggedCount = length $ filter isFlagged neighbours
                    , flaggedCount == countMines square
                    , let hiddenNeighbours = [(nx, ny) | (nx, ny) <- neighbourCoords x y size, isClearAndHidden (squares !! nx !! ny)]
                    , not (null hiddenNeighbours)
                    , let (nx, ny) = head hiddenNeighbours ]
    in listToMaybe safeMoves

-- Attempt to find a mine that can be flagged
findFlagMove :: [[Square]] -> Maybe (Int, Int)
findFlagMove squares = 
    let size = length squares
        coords = [(x, y) | x <- [0..size-1], y <- [0..size-1]]
        flagMoves = [ (nx, ny)
                    | (x, y) <- coords
                    , let square = squares !! x !! y
                    , isRevealed square
                    , let neighbours = getNeighbours squares x y
                    , let flaggedCount = length $ filter isFlagged neighbours
                    , let clearHiddenNeighbours = [(nx, ny) | (nx, ny) <- neighbourCoords x y size, isClearAndHidden (squares !! nx !! ny)]
                    , let clue = countMines square
                    , length clearHiddenNeighbours == clue - flaggedCount
                    , not (null clearHiddenNeighbours)
                    , let (nx, ny) = head clearHiddenNeighbours ]
    in listToMaybe flagMoves

-- Attempt to flag a mine using the 1-2-X pattern
find12XFlagMove :: [[Square]] -> Maybe (Int, Int)
find12XFlagMove squares = 
    let size = length squares
        coords = [(r, c) | r <- [0..size-1], c <- [0..size-1]]
        -- Check for the 1-2-X pattern
        flagMoves12X = [ (nr, nc)
                       | (r, c) <- coords
                       , Just (nr, nc) <- 
                            [ get12XRow squares r c (-1) 1
                            , get12XRow squares r c (-1) (-1)
                            , get12XRow squares r c 1    1
                            , get12XRow squares r c 1    (-1) 
                            , get12XCol squares r c (-1) 1
                            , get12XCol squares r c (-1) (-1)
                            , get12XCol squares r c 1    1
                            , get12XCol squares r c 1    (-1) ]]
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

-- Attempt to find the least dangerous move available
findLeastDangerousMove :: [[Square]] -> Maybe (Int, Int)
findLeastDangerousMove squares =
    let size = length squares
        coords = [(x, y) | x <- [0..size-1], y <- [0..size-1]]
        hiddenSquares = [ (x, y)
                        | (x, y) <- coords
                        , let square = squares !! x !! y
                        , not (isRevealed square)
                        , not (isFlagged square) ]
        -- Simplistic probability calculation: neighbours with mines / total neighbors
        probabilities = [ ((x, y), calculateProbability squares x y)
                        | (x, y) <- hiddenSquares ]
        safestMove = listToMaybe $ sortOn snd probabilities
    in fmap fst safestMove

-- Calculate a Square's probability (mines / total neighbours)
calculateProbability :: [[Square]] -> Int -> Int -> Double
calculateProbability squares x y =
    let neighbours = getNeighbours squares x y
        revealedNeighbours = filter isRevealed neighbours
        totalMines = sum [ countMines sq | sq <- revealedNeighbours ]
        totalHidden = length $ filter (not . isRevealed) neighbours
    in if totalHidden == 0 then 1.0 else fromIntegral totalMines / fromIntegral totalHidden

-- Check if a Square is Clear and Hidden
isClearAndHidden :: Square -> Bool
isClearAndHidden (Clear (Hidden _)) = True
isClearAndHidden _ = False

-- Get the neighbouring Squares of a given Square
getNeighbours :: [[Square]] -> Int -> Int -> [Square]
getNeighbours squares x y =
    [ squares !! nx !! ny
    | dx <- [-1..1], dy <- [-1..1], let nx = x + dx, let ny = y + dy
    , nx >= 0, ny >= 0, nx < length squares, ny < length (head squares)
    , (dx, dy) /= (0, 0) ]

-- Get the neighbouring coordinates of a given position
neighbourCoords :: Int -> Int -> Int -> [(Int, Int)]
neighbourCoords x y size =
    [ (nx, ny)
    | dx <- [-1..1], dy <- [-1..1]
    , let nx = x + dx, let ny = y + dy
    , nx >= 0, ny >= 0, nx < size, ny < size
    , (dx, dy) /= (0, 0) ]

-- Get the number of neighbouring mines of a given Square
countMines :: Square -> Int
countMines (Clear (Revealed (Empty n))) = n
countMines _ = 0

-- Test Grid for Debugging Advanced Solver Techniques
testGrid :: IO [[Square]]
testGrid = return [[(Clear (Hidden (Empty 2))), (Clear (Hidden Mine)),
                    (Clear (Hidden (Empty 2))), (Clear (Hidden Mine)),
                    (Clear (Hidden (Empty 2))), (Clear (Hidden Mine)),
                    (Clear (Hidden (Empty 2))), (Clear (Hidden Mine)),
                    (Clear (Hidden Mine)), (Clear (Hidden (Empty 1)))],
                   [(Clear (Hidden Mine)), (Clear (Revealed (Empty 2))),
                    (Clear (Revealed (Empty 2))), (Clear (Revealed (Empty 1))),
                    (Clear (Revealed (Empty 2))), (Clear (Revealed (Empty 1))),
                    (Clear (Revealed (Empty 2))), (Clear (Revealed (Empty 2))),
                    (Clear (Revealed (Empty 2))), (Clear (Revealed (Empty 1)))],
                   [(Clear (Hidden (Empty 2))), (Clear (Revealed (Empty 2))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0)))],
                   [(Clear (Hidden Mine)), (Clear (Revealed (Empty 1))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0)))],
                   [(Clear (Hidden (Empty 1))), (Clear (Revealed (Empty 1))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0)))],
                   [(Clear (Hidden (Empty 1))), (Clear (Revealed (Empty 1))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0)))],
                   [(Clear (Hidden Mine)), (Clear (Revealed (Empty 1))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0)))],
                   [(Clear (Hidden (Empty 2))), (Clear (Revealed (Empty 2))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0)))],
                   [(Clear (Hidden Mine)), (Clear (Revealed (Empty 1))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0)))],
                   [(Clear (Hidden (Empty 1))), (Clear (Revealed (Empty 1))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0))),
                    (Clear (Revealed (Empty 0))), (Clear (Revealed (Empty 0)))]]