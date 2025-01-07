module Minesweeper (module Minesweeper) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import System.Random

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding ((<|>), grid, style, row)

import Data.IORef
import Styles

-- Base Square Type (Mine or Empty)
data Cell
    = Mine
    | Empty Int
    deriving (Show, Eq)

-- State Square Type (Revealed or Hidden)
data State
    = Hidden Cell
    | Revealed Cell
    deriving (Show, Eq)

-- Full Square Type (Flagged or Unflagged)
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

-- Get the number of neighbouring mines of a square
getMines :: Square -> Int
getMines (Flagged (Hidden (Empty n)))   = n
getMines (Flagged (Revealed (Empty n))) = n
getMines (Clear (Hidden (Empty n)))     = n
getMines (Clear (Revealed (Empty n)))   = n
getMines _                              = (-1)

-- Check if a Square has N neighbouring Mines
hasNMines :: Square -> Int -> Bool
hasNMines square n = (getMines square == n)

-- Reveal a Square
reveal :: Square -> Square
reveal (Clear (Hidden cell)) = Clear (Revealed cell)
reveal square                = square

-- Reveal a Square (including flagged ones)
-- ONLY CALL THIS WHEN REVEALING MINES IN GAME OVER STATE
forceReveal :: Square -> Square
forceReveal (Flagged (Hidden cell)) = Clear (Revealed cell)
forceReveal (Clear (Hidden cell))   = Clear (Revealed cell)
forceReveal square                  = square

-- Check if a Square is Revealed
isRevealed :: Square -> Bool
isRevealed (Flagged (Revealed _)) = True
isRevealed (Clear (Revealed _))   = True
isRevealed _                      = False

-- Check if a Square is Hidden
isHidden :: Square -> Bool
isHidden (Flagged (Hidden _)) = True
isHidden (Clear (Hidden _))   = True
isHidden _                    = False

-- Flag a Square
flag :: Square -> Square
flag (Flagged (Hidden cell)) = Clear (Hidden cell)
flag (Clear (Hidden cell))   = Flagged (Hidden cell)
flag square                  = square

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
    let indices = [(r, c) | r <- [0..size-1], c <- [0..size-1]]
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
    [[ if (r, c) `elem` positions
        then Clear (Hidden Mine)
        else Clear (Hidden (Empty 0))
     | c <- [0..size-1]] 
     | r <- [0..size-1]]

-- Check if all Empty Squares have been Revealed
checkWin :: [[Square]] -> Bool
checkWin squares =
    let totalSquares = length squares * length (head squares)
        mineCount = 15
        revealedCount = length [() | row <- squares, Clear (Revealed _) <- row]
    in revealedCount == totalSquares - mineCount

-- Reveal all mines in the grid
revealMines :: IORef [[Element]] -> IORef [[Square]] -> UI ()
revealMines buttonsRef squaresRef = do
    buttons <- liftIO $ readIORef buttonsRef
    squares <- liftIO $ readIORef squaresRef
    forM_ [0..9] $ \i -> do
        forM_ [0..9] $ \j -> do
            let button = buttons !! i !! j
            let square = squares !! i !! j
            when (isMine square) $ do
                let newSquare = forceReveal square
                liftIO $ updateSquareInGrid squaresRef i j newSquare
                updateButton button newSquare

revealBadFlags :: IORef [[Element]] -> IORef [[Square]] -> UI ()
revealBadFlags buttonsRef squaresRef = do
    buttons <- liftIO $ readIORef buttonsRef
    squares <- liftIO $ readIORef squaresRef
    forM_ [0..9] $ \i -> do
        forM_ [0..9] $ \j -> do
            let button = buttons !! i !! j
            let square = squares !! i !! j
            when (isFlagged square && not (isMine square)) $ do
                void $ element button # set UI.style badFlagStyle

-- Calculate the Value of each Square (i.e. number of neighbouring mines)
calcNeighbours :: [[Square]] -> [[Square]]
calcNeighbours grid = 
    [[ if isMine (grid !! r !! c)
        then grid !! r !! c
        else let neighbours = countAdj grid r c
             in updateSquare (grid !! r !! c) neighbours
     | c <- [0..length grid - 1]]
     | r <- [0..length grid - 1]]
    where 
        countAdj g r c = 
            length [() | dr <- [-1..1], dc <- [-1..1],
                         let nr = r + dr, 
                         let nc = c + dc,
                         nr >= 0, nr < length g,
                         nc >= 0, nc < length (head g),
                         isMine (g !! nr !! nc)]
        updateSquare (Clear (Hidden (Empty _))) n = Clear (Hidden (Empty n))
        updateSquare square _                     = square

-- Reveal Neighbours of a Revealed Empty Square
revealNeighbours :: IORef [[Square]] -> Int -> Int -> UI ()
revealNeighbours squaresRef r c = do
    squares <- liftIO $ readIORef squaresRef
    forM_ [(-1)..1] $ \dr -> do
        forM_ [(-1)..1] $ \dc -> do
            let nr = r + dr
            let nc = c + dc
            -- Ensure neighbour is in bounds
            when (nr >= 0 && nc >= 0 && nr < length squares && nc < length (head squares)) $ do
                let neighbour = squares !! nr !! nc
                -- Reveal the square if not already revealed
                unless (isRevealed neighbour) $ do
                    let newNeighbour = reveal neighbour
                    liftIO $ updateSquareInGrid squaresRef nr nc newNeighbour
                    -- Recursively reveal neighbours if the neighbour is empty
                    when (isEmpty newNeighbour) $ revealNeighbours squaresRef nr nc

-- Helper function to update a square in the grid
updateSquareInGrid :: IORef [[Square]] -> Int -> Int -> Square -> IO ()
updateSquareInGrid squaresRef r c newSquare = do
    squares <- liftIO $ readIORef squaresRef
    let updatedRow = take c (squares !! r) ++ [newSquare] ++ drop (c + 1) (squares !! r)
    let updatedGrid = take r squares ++ [updatedRow] ++ drop (r + 1) squares
    liftIO $ writeIORef squaresRef updatedGrid

-- Create a new Button with corresponding Square
mkButton :: IORef [[Square]] -> IORef Bool -> Int -> Int -> UI (Element, Element)
mkButton squaresRef gameState i j = do
    -- Create the Button Element and Style
    button <- UI.button #. "button" # set UI.style hiddenStyle
    
    -- On Left Click we Reveal the Square
    on UI.click button $ \_ -> do
        -- Check that the game state is active first
        active <- liftIO $ readIORef gameState
        when active $ do
            -- Reveal the Square
            squares <- liftIO $ readIORef squaresRef
            let square = squares !! i !! j
            let newSquare = reveal square
            liftIO $ updateSquareInGrid squaresRef i j newSquare
            updateButton button newSquare
            -- If Square was Empty then reveal all neighbours
            when (isEmpty newSquare) $ revealNeighbours squaresRef i j
    
    -- On Right Click we Flag/Unflag the Square
    on UI.contextmenu button $ \_ -> do
        -- Check that the game state is active first
        active <- liftIO $ readIORef gameState
        when active $ do
            squares <- liftIO $ readIORef squaresRef
            let square = squares !! i !! j
            let newSquare = flag square
            liftIO $ updateSquareInGrid squaresRef i j newSquare
            updateButton button newSquare
    
    -- Return button with view
    view <- UI.div #+ [element button]
    return (button, view)

-- Update the Button Appearance depending on State
updateButton :: Element -> Square -> UI ()
updateButton button square = do
    let (icon, style) = case square of
            Flagged _                  -> ("X", hiddenStyle)
            Clear (Revealed Mine)      -> ("☀︎", mineStyle)
            Clear (Revealed (Empty 0)) -> ("", revealedStyle)
            Clear (Revealed (Empty n)) -> (show n, revealedStyle)
            _                          -> ("", hiddenStyle)
    let colour = case square of
            Flagged (Hidden _)         -> [("color", "red")]
            Clear (Revealed Mine)      -> [("color", "black")]
            Clear (Revealed (Empty 1)) -> [("color", "blue")]
            Clear (Revealed (Empty 2)) -> [("color", "limegreen")]
            Clear (Revealed (Empty 3)) -> [("color", "orangered")]
            Clear (Revealed (Empty 4)) -> [("color", "#8f00cb")]
            Clear (Revealed (Empty 5)) -> [("color", "orange")]
            Clear (Revealed (Empty 6)) -> [("color", "aqua")]
            Clear (Revealed (Empty 7)) -> [("color", "darkslategrey")]
            Clear (Revealed (Empty 8)) -> [("color", "black")]
            _                          -> []
    void $ element button # set UI.text icon # set UI.style (style ++ colour)

-- Create the Grid of Buttons
mkButtons :: Window -> IORef [[Square]] -> IORef Bool -> Element -> UI [Element]
mkButtons w squaresRef gameState message = do
    -- Create an array to store the buttons
    buttonsRef <- liftIO $ newIORef []
    -- Create 10 Columns
    rows <- forM [0..9] $ \i -> do
        -- Create 10 Rows
        rowButtons <- forM [0..9] $ \j -> do
            (b, v) <- mkButton squaresRef gameState i j
            -- Update the buttonsRef with the new button
            liftIO $ modifyIORef buttonsRef (\g ->
                if length g > i
                    then take i g ++ [(g !! i) ++ [b]] ++ drop (i + 1) g
                    else g ++ [[b]])
            return $ element v
        UI.div # set UI.style rowStyle #+ rowButtons
    grid <- UI.div # set UI.style gridStyle
        #+ map element rows
    void $ liftIO $ forkIO $ runUI w $ loop buttonsRef squaresRef gameState message 
    return [grid]

-- Loop to keep grid appearance updated
loop :: IORef [[Element]] -> IORef [[Square]] -> IORef Bool -> Element -> UI ()
loop buttonsRef squaresRef gameState message = do
    active <- liftIO $ readIORef gameState
    when active $ do
        updateGrid buttonsRef squaresRef gameState message
        liftIO $ threadDelay 100000
        loop buttonsRef squaresRef gameState message

-- Update the grid and check win/lose conditions
updateGrid :: IORef [[Element]] -> IORef [[Square]] -> IORef Bool -> Element -> UI ()
updateGrid buttonsRef squaresRef gameState message = do 
    buttons <- liftIO $ readIORef buttonsRef
    squares <- liftIO $ readIORef squaresRef
    gameOverRef <- liftIO $ newIORef False
    forM_ [0..9] $ \i -> do
        forM_ [0..9] $ \j -> do
            let button = buttons !! i !! j
            let square = squares !! i !! j
            updateButton button square
            -- If Square is a Revealed Mine set End Game Flag
            when (isMine square && isRevealed square) $ do
                void $ element message # set UI.style (messageStyle ++ [("color", "red")])
                void $ element message # set UI.text "Game Over."
                liftIO $ writeIORef gameOverRef True
    -- If End Game Flag is set then end the game
    gameOver <- liftIO $ readIORef gameOverRef
    when gameOver $ do
        revealMines buttonsRef squaresRef
        revealBadFlags buttonsRef squaresRef
        liftIO $ writeIORef gameState False
    -- If all Empty Squares Revealed then End the Game
    when (checkWin squares) $ do
        void $ element message # set UI.style (messageStyle ++ [("color", "yellow")])
        void $ element message # set UI.text "Congratulations!"
        liftIO $ writeIORef gameState False
