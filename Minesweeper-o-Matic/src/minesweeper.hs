module Minesweeper ( mkButtons, createGrid ) where

import Control.Monad
import System.Random

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding ((<|>), grid, style, row)

import Data.IORef

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

-- Reveal a Square
reveal :: Square -> Square
reveal (Clear (Hidden cell))   = Clear (Revealed cell)
reveal square                  = square

-- Check if a Square is Revealed
isRevealed :: Square -> Bool
isRevealed (Flagged (Revealed _)) = True
isRevealed (Clear (Revealed _))   = True
isRevealed _                      = False

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

-- Check if all Empty Squares have been Revealed
checkWin :: [[Square]] -> Bool
checkWin squares =
    let totalSquares = length squares * length (head squares)
        mineCount = 15
        revealedCount = length [() | row <- squares, Clear (Revealed _) <- row]
    in revealedCount == totalSquares - mineCount

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
revealNeighbours :: IORef [[Square]] -> IORef Bool -> Element -> Int -> Int -> UI ()
revealNeighbours squaresRef gameState message x y = do
    squares <- liftIO $ readIORef squaresRef
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
                    (button, _) <- mkButton squaresRef gameState message nx ny
                    updateButton button newNeighbour
                    -- Recursively reveal neighbours if the neighbour is empty
                    when (isEmpty newNeighbour) $ revealNeighbours squaresRef gameState message nx ny

-- Helper function to update a square in the grid
updateSquareInGrid :: IORef [[Square]] -> Int -> Int -> Square -> IO ()
updateSquareInGrid squaresRef x y newSquare = do
    squares <- liftIO $ readIORef squaresRef
    let updatedRow = take y (squares !! x) ++ [newSquare] ++ drop (y + 1) (squares !! x)
    let updatedGrid = take x squares ++ [updatedRow] ++ drop (x + 1) squares
    liftIO $ writeIORef squaresRef updatedGrid

-- Create a new Button with corresponding Square
mkButton :: IORef [[Square]] -> IORef Bool -> Element -> Int -> Int -> UI (Element, Element)
mkButton squaresRef gameState message i j = do
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
            -- If Square was an Unflagged Mine then End the Game
            if isMine square && not (isFlagged square)
                then do
                    liftIO $ writeIORef gameState False
                    _ <- element message # set UI.style (messageStyle ++ [("color", "red")])
                    _ <- element message # set UI.text "Game Over."
                    liftIO $ writeIORef gameState False
                else do
                    -- If all Empty Squares Revealed then End the Game
                    newSquares <- liftIO $ readIORef squaresRef
                    if checkWin newSquares
                        then do
                            liftIO $ writeIORef gameState False
                            _ <- element message # set UI.style (messageStyle ++ [("color", "yellow")])
                            _ <- element message # set UI.text "Congratulations!"
                            liftIO $ writeIORef gameState False
                        else do
                            -- If Square was Empty then reveal all neighbours
                            when (isEmpty newSquare) $ revealNeighbours squaresRef gameState message i j

    -- On Mouseover we Update the Square
    on UI.mousemove button $ \_ -> do
        -- Check that the game state is active first
        active <- liftIO $ readIORef gameState
        when active $ do
            squares <- liftIO $ readIORef squaresRef
            let square = squares !! i !! j
            updateButton button square
    
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
    
    view   <- UI.div #+ [element button]
    return (button, view)

-- Update the Button Text depending on State
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
            _                          -> []
    _ <- element button # set UI.text icon # set UI.style (style ++ colour)
    return ()

-- Create the Grid of Buttons
mkButtons :: IORef [[Square]] -> IORef Bool -> Element -> UI [Element]
mkButtons squaresRef gameState message = do
    -- Create 10 Columns
    rows <- forM [0..9] $ \i -> do
        -- Create 10 Rows
        rowButtons <- forM [0..9] $ \j -> do
            (_, v) <- mkButton squaresRef gameState message i j
            return $ element v
        UI.div # set UI.style rowStyle #+ rowButtons
    grid <- UI.div # set UI.style gridStyle
        #+ map element rows
    return [grid]

-- CSS Styles
hiddenStyle :: [(String, String)]
hiddenStyle   = [("width", "25px"), ("height", "25px"),
                 ("border-left", "3px solid white"),
                 ("border-top", "3px solid white"),
                 ("border-right", "3px solid #999999"),
                 ("border-bottom", "3px solid #999999"), ("outline", "none"),
                 ("background-color", "lightgrey"), ("font-weight", "bold")]

revealedStyle :: [(String, String)]
revealedStyle = [("width", "25px"), ("height", "25px"),
                 ("border", "1px solid #555555"), ("outline", "none"),
                 ("background-color", "grey"), ("font-weight", "bold")]

mineStyle :: [(String, String)]
mineStyle     = [("width", "25px"), ("height", "25px"),
                 ("border-color", "darkred"), ("border-width", "medium"),
                 ("background-color", "red"), ("font-weight", "bold"), ("font-size", "x-small")]

rowStyle :: [(String, String)]
rowStyle      = [("display", "inline-flex"), ("height", "25px"), ("width", "250px")]

gridStyle :: [(String, String)]
gridStyle     = [("display", "flex"), ("flex-direction", "column"), ("width", "250px"),
                 ("border-left", "10px solid #555555"),
                 ("border-top", "10px solid #555555"),
                 ("border-right", "10px solid #222222"),
                 ("border-bottom", "10px solid #222222")]

messageStyle :: [(String, String)]
messageStyle  = [("font-size", "xx-large"), ("font-weight", "bold"),
                 ("display", "flex"), ("justify-content", "center"), ("align-items", "center"),
                 ("width", "250px"), ("height", "50px"), ("background-color", "#333333"),
                 ("border-left", "10px solid #222222"),
                 ("border-top", "10px solid #222222"),
                 ("border-right", "10px solid black"),
                 ("border-bottom", "10px solid black"), ("margin-top", "10px")]