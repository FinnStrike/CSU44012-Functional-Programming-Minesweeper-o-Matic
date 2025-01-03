import Control.Monad
import Control.Concurrent (threadDelay)

import System.Directory

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding ((<|>))

import Data.IORef

-- User Interface

main :: IO ()
main = do
    static <- getCurrentDirectory
    startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Minesweeper-o-Matic"
    -- Prevent Default Context Menu from showing on Right Click
    runFunction $ ffi "document.addEventListener('contextmenu', function(e) { e.preventDefault(); })"
    -- Create Grid of Buttons
    squaresRef <- liftIO $ newIORef createGrid
    buttons <- mkButtons squaresRef
    -- Display Grid
    getBody w #+ [UI.div #. "wrap" #+ (greet ++ map element buttons)]

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Minesweeper-o-Matic"]
    ]

-- Create a new Button with corresponding Square
mkButton :: IORef Square -> UI (Element, Element)
mkButton squareRef = do
    -- Create the Button Element and Style
    button <- UI.button #. "button" # set UI.style [("width", "25px"), ("height", "25px")]
    -- On Left Click we Reveal the Square
    on UI.click button $ \_ -> do
        currentSquare <- liftIO $ readIORef squareRef
        let newSquare = case currentSquare of
                Clear (Hidden cell)   -> Clear (Revealed cell)
                _                     -> currentSquare
        liftIO $ writeIORef squareRef newSquare
        updateButton button newSquare 
    -- On Right Click we Flag/Unflag the Square
    on UI.contextmenu button $ \evt -> do
        currentSquare <- liftIO $ readIORef squareRef
        let newSquare = case currentSquare of
                Clear (Hidden cell)   -> Flagged (Hidden cell)
                Flagged (Hidden cell) -> Clear (Hidden cell)
                _                     -> currentSquare
        liftIO $ writeIORef squareRef newSquare
        updateButton button newSquare
    view   <- UI.div #+ [element button]
    return (button, view)

-- Update the Button Text depending on State
updateButton :: Element -> Square -> UI Element
updateButton button square = do
    let title = case square of
            Flagged _                  -> "X"
            Clear (Revealed Mine)      -> "*"
            Clear (Revealed (Empty n)) -> show n
            _                          -> ""
    element button # set UI.text title

-- Create the Grid of Buttons
mkButtons :: IORef [[Square]] -> UI [Element]
mkButtons squaresRef = do
    -- Create 10 Columns
    rows <- forM [0..9] $ \i -> do
        -- Create 10 Rows
        rowButtons <- forM [0..9] $ \j -> do
            squares <- liftIO $ readIORef squaresRef
            squareRef <- liftIO $ newIORef (squares !! i !! j)
            (b, v) <- mkButton squareRef
            return $ element v
        UI.div # set UI.style [("display", "inline-flex")] #+ rowButtons
    grid <- UI.div # set UI.style [("display", "flex"), ("flex-direction", "column")]
        #+ map element rows
    return [grid]

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

-- Reveal a Square
reveal :: Square -> Square
reveal (Flagged (Hidden cell)) = Clear (Revealed cell)
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

-- Create a 10x10 Grid of Squares
createGrid :: [[Square]]
createGrid = replicate 10 (replicate 10 (Clear (Hidden (Empty 0))))