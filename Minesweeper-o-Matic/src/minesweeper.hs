module Minesweeper where

import Control.Monad
import Control.Concurrent (threadDelay)
import System.Random

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding ((<|>))

import Data.IORef

-- Base Square Type
data Cell
    = Mine
    | Empty Int
    deriving (Show, Eq)

-- State Square Type
data State
    = Hidden Cell
    | Revealed Cell
    deriving (Show, Eq)

-- Full Square Type
data Square
    = Flagged State
    | Clear State
    deriving (Show, Eq)

-- Check if a Square is a Mine
isMine :: Square -> Bool
isMine (Flagged (Hidden Mine))   = True
isMine (Flagged (Revealed Mine)) = True
isMine (Clear (Hidden Mine))     = True
isMine (Clear (Revealed Mine))   = True
isMine _                         = False

-- Check if a Square has no neighbouring Mines
isEmpty :: Square -> Bool
isEmpty (Clear (Revealed (Empty 0))) = True
isEmpty _                            = False

-- Reveal a Square
reveal :: Square -> Square
reveal (Clear (Hidden cell))   = Clear (Revealed cell)
reveal square                   = square

-- Check if a Square is Revealed
isRevealed :: Square -> Bool
isRevealed (Flagged (Revealed _)) = True
isRevealed (Clear (Revealed _))   = True
isRevealed _                      = False

-- Flag a Square
flag :: Square -> Square
flag (Flagged (Hidden cell)) = Clear (Hidden cell)
flag (Clear (Hidden cell))   = Flagged (Hidden cell)
flag square                   = square

-- Check if a Square is Flagged
isFlagged :: Square -> Bool
isFlagged (Flagged _) = True
isFlagged _           = False

-- Create a Random 10x10 Grid of Squares with 15 Mines
createGrid :: IO [[Square]]
createGrid = do
    -- Generate 15 Random Mine Positions
    let size = 10
        count = 15
    positions <- generateMines size count
    -- Initialize Grid with Mines
    let initialGrid = placeMines size positions
    return $ calcNeighbours initialGrid

-- Generate Random Mine Positions
generateMines :: Int -> Int -> IO [(Int, Int)]
generateMines size count = do
    let indices = [(x, y) | x <- [0..size-1], y <- [0..size-1]]
    selected <- shuffle indices
    return $ take count selected

-- Shuffle a list of coordinates
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    i <- randomRIO (0, length xs - 1)
    rest <- shuffle (take i xs ++ drop (i+1) xs)
    return $ xs !! i : rest

-- Place the Mines into the Grid
placeMines :: Int -> [(Int, Int)] -> [[Square]]
placeMines size positions = 
    [[ if (x, y) `elem` positions
        then Clear (Hidden Mine)
        else Clear (Hidden (Empty 0))
     | y <- [0..size-1]] 
     | x <- [0..size-1]]

-- Calculate the Value of each Square (i.e. number of neighbouring mines)
calcNeighbours :: [[Square]] -> [[Square]]
calcNeighbours grid = 
    [[ if isMine (grid !! x !! y)
        then grid !! x !! y
        else let neighbours = countAdj grid x y
             in updateSquare (grid !! x !! y) neighbours
     | y <- [0..length grid - 1]]
     | x <- [0..length grid - 1]]
    where 
        countAdj g x y = 
            length [() | dx <- [-1..1], dy <- [-1..1],
                         let nx = x + dx, 
                         let ny = y + dy,
                         nx >= 0, nx < length g,
                         ny >= 0, ny < length (head g),
                         isMine (g !! nx !! ny)]
        updateSquare (Clear (Hidden (Empty _))) n = Clear (Hidden (Empty n))
        updateSquare square _                     = square

-- Reveal Neighbours of a Revealed Empty Square
revealNeighbours :: IORef [[Square]] -> Int -> Int -> UI ()
revealNeighbours squaresRef x y = do
    squares <- liftIO $ readIORef squaresRef
    let square = squares !! x !! y
    -- If Square is Revealed and Empty, Reveal Neighbours
    when (isEmpty (reveal square)) $ do
        forM_ [(-1)..1] $ \dx -> do
            forM_ [(-1)..1] $ \dy -> do
                let nx = x + dx
                let ny = y + dy
                -- Ensure neighbour is in bounds
                when (nx >= 0 && ny >= 0 && nx < length squares && ny < length (head squares)) $ do
                    let neighbour = squares !! nx !! ny
                    -- Reveal the square if not already revealed
                    unless (isRevealed neighbour) $ do
                        let newNeighbour = reveal neighbour
                        liftIO $ updateSquareInGrid squaresRef nx ny newNeighbour
                        (button, _) <- mkButton squaresRef nx ny
                        updateButton button newNeighbour
                        -- Recursively reveal neighbours if the neighbour is empty
                        when (isEmpty newNeighbour) $ revealNeighbours squaresRef nx ny

-- Helper function to update a square in the grid
updateSquareInGrid :: IORef [[Square]] -> Int -> Int -> Square -> IO ()
updateSquareInGrid squaresRef x y newSquare = do
    squares <- liftIO $ readIORef squaresRef
    let updatedRow = take y (squares !! x) ++ [newSquare] ++ drop (y + 1) (squares !! x)
    let updatedGrid = take x squares ++ [updatedRow] ++ drop (x + 1) squares
    liftIO $ writeIORef squaresRef updatedGrid

-- Create a new Button with corresponding Square
mkButton :: IORef [[Square]] -> Int -> Int -> UI (Element, Element)
mkButton squaresRef i j = do
    -- Create the Button Element and Style
    button <- UI.button #. "button" # set UI.style [("width", "25px"), ("height", "25px")]
    
    -- On Left Click we Reveal the Square
    on UI.click button $ \_ -> do
        squares <- liftIO $ readIORef squaresRef
        let square = squares !! i !! j
        let newSquare = reveal square
        liftIO $ updateSquareInGrid squaresRef i j newSquare
        updateButton button newSquare
        when (isEmpty newSquare) $ revealNeighbours squaresRef i j

    -- On Mouseover we Update the Square
    on UI.mousemove button $ \_ -> do
        squares <- liftIO $ readIORef squaresRef
        let square = squares !! i !! j
        updateButton button square
    
    -- On Right Click we Flag/Unflag the Square
    on UI.contextmenu button $ \_ -> do
        squares <- liftIO $ readIORef squaresRef
        let square = squares !! i !! j
        let newSquare = flag square
        liftIO $ updateSquareInGrid squaresRef i j newSquare
        updateButton button newSquare
    
    view   <- UI.div #+ [element button]
    return (button, view)

-- Update the Button Text depending on State
updateButton :: Element -> Square -> UI Element
updateButton button square = do
    let icon = case square of
            Flagged _                  -> "X"
            Clear (Revealed Mine)      -> "*"
            Clear (Revealed (Empty n)) -> show n
            _                          -> ""
    element button # set UI.text icon

-- Create the Grid of Buttons
mkButtons :: IORef [[Square]] -> UI [Element]
mkButtons squaresRef = do
    -- Create 10 Columns
    rows <- forM [0..9] $ \i -> do
        -- Create 10 Rows
        rowButtons <- forM [0..9] $ \j -> do
            squares <- liftIO $ readIORef squaresRef
            (_, v) <- mkButton squaresRef i j
            return $ element v
        UI.div # set UI.style [("display", "inline-flex"), ("height", "25px")] #+ rowButtons
    grid <- UI.div # set UI.style [("display", "flex"), ("flex-direction", "column")]
        #+ map element rows
    return [grid]