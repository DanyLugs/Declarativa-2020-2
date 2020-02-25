module String where

import Data.Char    
import Data.List

type Text = String
type Word = Char    
 
quitaMayusculas :: Text -> Text
quitaMayusculas (x:xs) = [x | x <- (x:xs), notElem x ['A'..'Z']]

soloLetras :: Text -> Text
soloLetras (x:xs) = [x | x <- (x:xs), elem x ['a'..'z'] || elem x ['A'..'Z']]

prefijo :: Text -> Text -> Bool
prefijo xs ys = isPrefixOf xs ys