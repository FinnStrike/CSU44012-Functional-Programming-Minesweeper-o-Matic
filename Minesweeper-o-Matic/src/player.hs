module Player where

import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding ((<|>), grid, style, row)

import Data.IORef
import Data.Maybe
import Data.List
import Minesweeper

-- Attempt to play the best move available
playMove :: IORef [[Square]] -> IORef Bool -> Element -> UI ()
playMove squaresRef gameState message = do
    squares <- liftIO $ readIORef squaresRef
    active <- liftIO $ readIORef gameState
    when active $ do
        -- Attempt to find a safe move
        case findSafeMove squares of
            Just (x, y) -> do
                -- Reveal the safe square
                let square = squares !! x !! y
                let newSquare = reveal square
                liftIO $ updateSquareInGrid squaresRef x y newSquare
                (button, _) <- mkButton squaresRef gameState message x y
                updateButton button newSquare
                when (isEmpty newSquare) $ revealNeighbours squaresRef gameState message x y
            Nothing -> do
                -- If no safe move is found, pick the least dangerous move
                case findLeastDangerousMove squares of
                    Just (x, y) -> do
                        let square = squares !! x !! y
                        let newSquare = reveal square
                        liftIO $ updateSquareInGrid squaresRef x y newSquare
                        (button, _) <- mkButton squaresRef gameState message x y
                        updateButton button newSquare
                        when (isEmpty newSquare) $ revealNeighbours squaresRef gameState message x y
                    Nothing -> do
                        -- If no moves are available, display a message
                        _ <- element message # set UI.text "No moves available."
                        return ()

-- Attempt to find an unambiguously safe move
findSafeMove :: [[Square]] -> Maybe (Int, Int)
findSafeMove squares = 
    let size = length squares
        coords = [(x, y) | x <- [0..size-1], y <- [0..size-1]]
        safeMoves = [ (x, y)
                    | (x, y) <- coords
                    , let square = squares !! x !! y
                    , isRevealed square
                    , let neighbours = getNeighbours squares x y
                    , let flaggedCount = length $ filter isFlagged neighbours
                    , let hiddenCount = length $ filter (not . isRevealed) neighbours
                    , flaggedCount == countMines square
                    , hiddenCount > 0
                    , any (not . isRevealed) neighbours ]
    in listToMaybe safeMoves

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

-- Get the neighbouring Squares of a given Square
getNeighbours :: [[Square]] -> Int -> Int -> [Square]
getNeighbours squares x y =
    [ squares !! nx !! ny
    | dx <- [-1..1], dy <- [-1..1], let nx = x + dx, let ny = y + dy
    , nx >= 0, ny >= 0, nx < length squares, ny < length (head squares)
    , (dx, dy) /= (0, 0) ]

-- Get the number of neighbouring mines of a given Square
countMines :: Square -> Int
countMines (Clear (Revealed (Empty n))) = n
countMines _ = 0