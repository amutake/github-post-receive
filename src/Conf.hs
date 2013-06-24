module Conf where

data Conf = Conf
    { appName :: String
    , scriptPaths :: [FilePath]
    } deriving (Eq, Show, Read)

parse :: String -> [Conf]
parse = undefined
