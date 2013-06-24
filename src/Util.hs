module Util where

import Data.Char

lowerCamelCase :: String -> String
lowerCamelCase "" = ""
lowerCamelCase (s:ss) = toLower s : ss

dataFieldToKeyName :: String -> String -> String
dataFieldToKeyName toDrop = lowerCamelCase . drop (length toDrop)
