import Control.Monad
import Control.Concurrent (forkIO, threadDelay)

import System.Directory

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding ((<|>))

import Data.IORef
import Minesweeper
import Player
import Styles

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
    void $ element resetButton # set UI.style resetStyle
    on UI.click resetButton $ \_ -> do
        void $ getBody w # set children []
        newGame <- createGame w
        newContainer <- UI.div # set UI.style containerStyle
            #+ [UI.div # set UI.style gameStyle #+ newGame, element resetButton]
        getBody w #+ (greet ++ [return newContainer])
    -- Set up the Game
    game <- createGame w
    container <- UI.div # set UI.style containerStyle
        #+ [UI.div # set UI.style gameStyle #+ game, element resetButton]
    -- Display Grid
    getBody w #+ (greet ++ [return container])

createGame :: Window -> UI ([UI Element])
createGame w = do
    -- Create Grid of Squares
    squaresRef <- liftIO $ newIORef =<< createGrid
    -- Set Game State to True
    gameState <- liftIO $ newIORef True
    -- Set up Game Message
    message <- UI.div #. "message" # set text ""
    -- Create Grid of Buttons
    buttons <- mkButtons w squaresRef gameState message
    -- Create Play Move Button
    playButton <- UI.button #+ [string "Play Move"]
    void $ element playButton # set UI.style playStyle
    on UI.click playButton $ \_ -> playMove squaresRef gameState
    -- Return Game
    return [UI.div #. "wrap" #+ ([element playButton] 
            ++ map element buttons 
            ++ [element message])]

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Minesweeper-o-Matic"]
    ]