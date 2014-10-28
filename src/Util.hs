module Util where

import Data.Char
import Data.Time.LocalTime
import Data.Time.Format
import System.IO (hFlush,stdout)
import System.Locale

lowerCamelCase :: String -> String
lowerCamelCase "" = ""
lowerCamelCase (s:ss) = toLower s : ss

dataFieldToKeyName :: String -> String -> String
dataFieldToKeyName toDrop = lowerCamelCase . drop (length toDrop)

logMessage :: String -> IO ()
logMessage s = do
  t <- getZonedTime
  putStrLn $ formatTime defaultTimeLocale "[%d/%b/%Y %T %z] " t ++ s
  hFlush stdout

