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
    -- Create Grid of Buttons
    squaresRef <- liftIO $ newIORef =<< createGrid
    gameState <- liftIO $ newIORef True
    buttons <- mkButtons squaresRef gameState
    -- Display Grid
    getBody w #+ [UI.div #. "wrap" #+ (greet ++ map element buttons)]

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Minesweeper-o-Matic"]
    ]