module Styles (module Styles) where

-- CSS Styles
hiddenStyle :: [(String, String)]
hiddenStyle = [("width", "25px"), ("height", "25px"),
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
mineStyle = [("width", "25px"), ("height", "25px"),
             ("border-color", "darkred"), ("border-width", "medium"),
             ("background-color", "red"), ("font-weight", "bold"), ("font-size", "x-small")]

badFlagStyle :: [(String, String)]
badFlagStyle = [("width", "25px"), ("height", "25px"),
                ("border-color", "#bd0b0b #4a0202 #4a0202 #bd0b0b"),
                ("border-width", "3px"), ("outline", "none"),
                ("background-color", "maroon"), ("font-weight", "bold")]

rowStyle :: [(String, String)]
rowStyle = [("display", "inline-flex"), ("height", "25px"), ("width", "250px")]

gridStyle :: [(String, String)]
gridStyle = [("display", "flex"), ("flex-direction", "column"), ("width", "250px"),
             ("border-left", "10px solid #555555"),
             ("border-top", "10px solid #555555"),
             ("border-right", "10px solid #222222"),
             ("border-bottom", "10px solid #222222")]

messageStyle :: [(String, String)]
messageStyle = [("font-size", "xx-large"), ("font-weight", "bold"),
                ("display", "flex"), ("justify-content", "center"), ("align-items", "center"),
                ("width", "250px"), ("height", "50px"), ("background-color", "#333333"),
                ("border-left", "10px solid #222222"),
                ("border-top", "10px solid #222222"),
                ("border-right", "10px solid black"),
                ("border-bottom", "10px solid black"), ("margin-top", "10px")]

playStyle :: [(String, String)]
playStyle = [("width", "200px"), ("height", "35px"), 
             ("margin", "0px 30px 20px"), ("border-radius", "20px"),
             ("font-weight", "bold"), ("font-family", "system-ui"),
             ("font-size", "11pt"), 
             ("background", "linear-gradient(15deg, grey, transparent)")]

resetStyle :: [(String, String)]
resetStyle = [("margin-left", "20px"), ("margin-top", "55px")]

containerStyle :: [(String, String)]
containerStyle = [("display", "flex"), ("align-items", "flex-start")]

gameStyle :: [(String, String)]
gameStyle = [("width", "300px")]