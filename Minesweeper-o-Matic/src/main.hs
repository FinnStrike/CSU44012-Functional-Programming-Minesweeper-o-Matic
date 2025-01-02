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
    UI.addStyleSheet w "grid.css"

    buttons <- mkButtons
    getBody w #+ [UI.div #. "wrap" #+ (greet ++ map element buttons)]

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Minesweeper-o-Matic"]
    ]

mkButton :: String -> UI (Element, Element)
mkButton title = do
    stateRef <- liftIO $ newIORef False
    button <- UI.button #. "button" #+ [string title]
                # set UI.style [("width", "25px"), ("height", "25px")]
    on UI.click button $ \_ -> do
        currentState <- liftIO $ readIORef stateRef
        liftIO $ writeIORef stateRef (not currentState)
        let newTitle = if currentState then "" else "X"
        element button # set UI.text newTitle
    view   <- UI.div #+ [element button]
    return (button, view)

mkButtons :: UI [Element]
mkButtons = do
    rows <- forM [1..10] $ \_ -> do
        rowButtons <- forM [1..10] $ \_ -> do
            (b, v) <- mkButton ""
            return $ element v
        UI.div # set UI.style [("display", "inline-flex")] #+ rowButtons
    grid <- UI.div # set UI.style [("display", "flex"), ("flex-direction", "column")]
        #+ map element rows

    return [grid]

data Block
    = Mine
    | Empty Int
    deriving (Show, Eq)

data State
    = Hidden Block
    | Revealed Block
    deriving (Show, Eq)

data Square
    = Flagged State
    | Clear State
    deriving (Show, Eq)

isMine :: Square -> Bool
isMine (Flagged (Hidden Mine))   = True
isMine (Flagged (Revealed Mine)) = True
isMine (Clear (Hidden Mine))     = True
isMine (Clear (Revealed Mine))   = True
isMine _                         = False

reveal :: Square -> Square
reveal (Flagged (Hidden block)) = Clear (Revealed block)
reveal (Clear (Hidden block))   = Clear (Revealed block)
reveal square                   = square

isRevealed :: Square -> Bool
isRevealed (Flagged (Revealed _)) = True
isRevealed (Clear (Revealed _))   = True
isRevealed _                      = False

flag :: Square -> Square
flag (Flagged (Hidden block)) = Clear (Hidden block)
flag (Clear (Hidden block))   = Flagged (Hidden block)
flag square                   = square

isFlagged :: Square -> Bool
isFlagged (Flagged _) = True
isFlagged _           = False