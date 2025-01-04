import Control.Monad
import Control.Concurrent (threadDelay)

import System.Directory

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding ((<|>))

import Data.IORef
import Minesweeper

-- User Interface

main :: IO ()
main = do
    static <- getCurrentDirectory
    startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
    _ <- return w # set title "Minesweeper-o-Matic"
    -- Prevent Default Context Menu from showing on Right Click
    runFunction $ ffi "document.addEventListener('contextmenu', function(e) { e.preventDefault(); })"
    -- Create Reset Button
    resetButton <- UI.button #+ [string "Reset"]
    void $ element resetButton # set UI.style [("margin-left", "20px")]
    on UI.click resetButton $ \_ -> do
        void $ getBody w # set children []
        newGame <- createGame
        newContainer <- UI.div # set UI.style [("display", "flex"), ("align-items", "flex-start")]
            #+ [UI.div # set UI.style [("width", "300px")] #+ newGame, element resetButton]
        getBody w #+ (greet ++ [return newContainer])
    -- Set up the Game
    game <- createGame
    container <- UI.div # set UI.style [("display", "flex"), ("align-items", "flex-start")]
        #+ [UI.div # set UI.style [("width", "300px")] #+ game, element resetButton]
    -- Display Grid
    getBody w #+ (greet ++ [return container])

createGame :: UI ([UI Element])
createGame = do
    -- Create Grid of Squares
    squaresRef <- liftIO $ newIORef =<< createGrid
    -- Set Game State to True
    gameState <- liftIO $ newIORef True
    -- Set up Game Message
    message <- UI.div #. "message" # set text ""
    -- Create Grid of Buttons
    buttons <- mkButtons squaresRef gameState message
    return [UI.div #. "wrap" #+ (map element buttons ++ [element message])]

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Minesweeper-o-Matic"]
    ]