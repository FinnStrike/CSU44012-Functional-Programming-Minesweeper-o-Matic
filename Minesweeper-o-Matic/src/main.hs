import Control.Monad
import Control.Concurrent (threadDelay)

import System.Directory

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding ((<|>))

import Data.IORef
import Text.Read (readMaybe)
import Text.Parsec hiding (string)
import Text.Parsec.String (Parser)

-- User Interface

main :: IO ()
main = do
    static <- getCurrentDirectory
    startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Calculator"
    UI.addStyleSheet w "calc.css"

    buttons <- mkButtons
    getBody w #+ [UI.div #. "wrap" #+ (greet ++ map element buttons)]

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "Calculator"]
    ]

-- Create a button with a text item, fixed width for consistency
mkButton :: String -> UI (Element, Element)
mkButton title = do
    button <- UI.button #. "button" #+ [string title]
              # set UI.style [("width", "25px")]
    view   <- UI.div #+ [element button]
    return (button, view)

-- In case we want the number buttons to stand out (it looked terrible so I don't)
mkNumButton :: String -> UI (Element, Element)
mkNumButton title = do
    button <- UI.button #. "button" #+ [string title]
              # set UI.style [("width", "25px")]
    view   <- UI.div #+ [element button]
    return (button, view)

-- Create all the calculator buttons and screen (and all the logic that comes with it)
mkButtons :: UI [Element]
mkButtons = do
    -- All the logic for creating and maintaining expressions
    exprRef <- liftIO $ newIORef ""

    str <- UI.div #. "output-container"
        # set UI.style [("border", "1px solid black"),
                        ("padding", "10px"),
                        ("min-height", "20px"),
                        ("width", "108px"),
                        ("text-align", "right"),
                        ("margin-bottom", "10px")]

    let updateExpr char = do
            currentExpr <- liftIO $ readIORef exprRef
            when (length currentExpr < 13) $ do
                liftIO $ modifyIORef exprRef (++ char)
                updatedExpr <- liftIO $ readIORef exprRef
                void $ element str # set html updatedExpr

    let clearExpr = do
            liftIO $ writeIORef exprRef ""
            void $ element str # set html ""

    let backExpr = do
            currentExpr <- liftIO $ readIORef exprRef
            let updatedExpr = if null currentExpr then "" else init currentExpr
            liftIO $ writeIORef exprRef updatedExpr
            void $ element str # set html updatedExpr

    let evaluateExpr = do
            currentExpr <- liftIO $ readIORef exprRef
            let result = evaluateExpression currentExpr
            liftIO $ writeIORef exprRef result
            void $ element str # set html result

    -- All the buttons...

    (nB1, vB1) <- mkButton "("
    on UI.click nB1 $ \_ -> updateExpr "("

    (nB2, vB2) <- mkButton ")"
    on UI.click nB2 $ \_ -> updateExpr ")"

    (nC, vC) <- mkButton "C"
    on UI.click nC $ \_ -> clearExpr

    (nB, vB) <- mkButton "←"
    on UI.click nB $ \_ -> backExpr

    (n1, v1) <- mkNumButton "1"
    on UI.click n1 $ \_ -> updateExpr "1"

    (n2, v2) <- mkNumButton "2"
    on UI.click n2 $ \_ -> updateExpr "2"

    (n3, v3) <- mkNumButton "3"
    on UI.click n3 $ \_ -> updateExpr "3"

    (nA, vA) <- mkButton "+"
    on UI.click nA $ \_ -> updateExpr "+"

    (n4, v4) <- mkNumButton "4"
    on UI.click n4 $ \_ -> updateExpr "4"

    (n5, v5) <- mkNumButton "5"
    on UI.click n5 $ \_ -> updateExpr "5"

    (n6, v6) <- mkNumButton "6"
    on UI.click n6 $ \_ -> updateExpr "6"

    (nS, vS) <- mkButton "-"
    on UI.click nS $ \_ -> updateExpr "-"

    (n7, v7) <- mkNumButton "7"
    on UI.click n7 $ \_ -> updateExpr "7"

    (n8, v8) <- mkNumButton "8"
    on UI.click n8 $ \_ -> updateExpr "8"

    (n9, v9) <- mkNumButton "9"
    on UI.click n9 $ \_ -> updateExpr "9"

    (nM, vM) <- mkButton "×"
    on UI.click nM $ \_ -> updateExpr "*"

    (nD, vD) <- mkButton "."
    on UI.click nD $ \_ -> updateExpr "."

    (n0, v0) <- mkNumButton "0"
    on UI.click n0 $ \_ -> updateExpr "0"

    (nE, vE) <- mkButton "="
    on UI.click nE $ \_ -> evaluateExpr

    (nF, vF) <- mkButton "÷"
    on UI.click nF $ \_ -> updateExpr "/"

    c0 <- UI.div # set UI.style [("display", "inline-flex"), ("gap", "10px")] #+ map element [vB1, vB2, vC, vB]
    c1 <- UI.div # set UI.style [("display", "inline-flex"), ("gap", "10px")] #+ map element [v1, v2, v3, vF]
    c2 <- UI.div # set UI.style [("display", "inline-flex"), ("gap", "10px")] #+ map element [v4, v5, v6, vM]
    c3 <- UI.div # set UI.style [("display", "inline-flex"), ("gap", "10px")] #+ map element [v7, v8, v9, vS]
    c4 <- UI.div # set UI.style [("display", "inline-flex"), ("gap", "10px")] #+ map element [vD, v0, vE, vA]

    cB <- UI.div # set UI.style [("display", "flex"), ("flex-direction", "column"), ("gap", "10px"), ("margin-top", "10px")]
        #+ map element [c0, c1, c2, c3, c4]

    return [str, cB]

-- Expression Parsing Logic (relies on parsec library)

data Expr
    = Add Expr Expr
    | Subtract Expr Expr
    | Multiply Expr Expr
    | Divide Expr Expr
    | Number Double
    deriving Show

parseExpr :: Parser Expr
parseExpr = try parseTerm `chainl1` (addOp <|> subOp)

parseTerm :: Parser Expr
parseTerm = try parseFactor `chainl1` (mulOp <|> divOp)

parseFactor :: Parser Expr
parseFactor = try parseNumber <|> parseParens

-- This should allow us to support decimals and negative numbers
parseNumber :: Parser Expr
parseNumber = do
    sign <- option "" (char '-' >> return "-")
    integerPart <- many1 digit
    decimalPart <- option "" (do 
                                char '.'
                                frac <- many1 digit
                                return $ '.' : frac
                              )
    let numberStr = sign ++ integerPart ++ decimalPart
    return $ Number (read numberStr :: Double)

parseParens :: Parser Expr
parseParens = do
    char '('
    e <- parseExpr
    char ')'
    return e

addOp :: Parser (Expr -> Expr -> Expr)
addOp = do
    char '+'
    return Add

subOp :: Parser (Expr -> Expr -> Expr)
subOp = do
    char '-'
    return Subtract

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = do
    char '*'
    return Multiply

divOp :: Parser (Expr -> Expr -> Expr)
divOp = do
    char '/'
    return Divide

parseInput :: String -> Either ParseError Expr
parseInput input = parse parseExpr "" input

-- Actually evaluate the string expressions
evaluate :: Expr -> Double
evaluate (Number n) = n
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Subtract e1 e2) = evaluate e1 - evaluate e2
evaluate (Multiply e1 e2) = evaluate e1 * evaluate e2
evaluate (Divide e1 e2) = evaluate e1 / evaluate e2

-- Give the result back as a string - present whole numbers as ints and clip to 13 chars max
evaluateExpression :: String -> String
evaluateExpression expr =
    case parseInput expr of
        Left err -> "Error"
        Right ast -> 
            let result = evaluate ast
                formattedResult = 
                    if result == fromIntegral (round result)
                    then show (round result :: Int)
                    else show result
                clippedResult = if length formattedResult > 13
                                then take 13 formattedResult
                                else formattedResult
            in clippedResult