import Control.Monad

import System.Directory

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding ((<|>))

import Data.IORef
import Minesweeper
import Player
import Styles

-- Main Loop to run Minesweeper
main :: IO ()
main = do
    static <- getCurrentDirectory
    startGUI defaultConfig { jsStatic = Just static } setup

-- Set up the window and run the game
setup :: Window -> UI ()
setup w = void $ do
    _ <- return w # set title "Minesweeper-o-Matic"
    -- Prevent Default Context Menu from showing on Right Click
    runFunction $ ffi "document.addEventListener('contextmenu', function(e) { e.preventDefault(); })"
    -- Create Reset Button
    resetButton <- UI.button #+ [string "Reset"]
    void $ element resetButton # set UI.style resetStyle
    -- When clicked, the Reset Button starts a new Game
    on UI.click resetButton $ \_ -> do
        -- Clear current Game
        void $ getBody w # set children []
        -- Set up the new Game
        newGame <- createGame w
        newContainer <- UI.div # set UI.style containerStyle
            #+ [UI.div # set UI.style gameStyle #+ newGame, element resetButton]
        -- Display Grid
        getBody w #+ (greet ++ [return newContainer])
    -- Set up the Game
    game <- createGame w
    container <- UI.div # set UI.style containerStyle
        #+ [UI.div # set UI.style gameStyle #+ game, element resetButton]
    -- Display Grid
    getBody w #+ (greet ++ [return container])

-- Initialize a Game of Minesweeper
createGame :: Window -> UI ([UI Element])
createGame w = do
    -- Create Grid of Squares
    squaresRef <- liftIO $ newIORef =<< createGrid
    -- Set Game State to Active
    gameState <- liftIO $ newIORef True
    -- Set up element for displaying Win/Loss
    message <- UI.div #. "message" # set text ""
    -- Create Grid of Buttons
    buttons <- mkButtons w squaresRef gameState message
    -- Create Play Move Button
    playButton <- UI.button #+ [string "✨ Play Move with AI"]
    void $ element playButton # set UI.style playStyle
    -- When clicked, the Play Move Button will automatically 
    -- play the (hopefully) best move
    -- DISCLAIMER: IT DOESN'T ACTUALLY USE AI TO CALCULATE THE MOVE
    on UI.click playButton $ \_ -> playMove squaresRef gameState
    -- Return the created Game
    return [UI.div #. "wrap" #+ ([element playButton] 
            ++ map element buttons 
            ++ [element message])]

-- Title of the Game
greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Minesweeper-o-Matic"]
    ]