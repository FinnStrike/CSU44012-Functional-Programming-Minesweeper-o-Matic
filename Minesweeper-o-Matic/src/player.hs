module Player (module Player) where

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
                -- Attemp to find a flag move
                case findFlagMove squares of
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