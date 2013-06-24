module Main where

import Safe
import System.Environment
import Web.Scotty

import Conf
import Receiver

main :: IO ()
main = do
    args <- getArgs
    case headMay args of
        Nothing -> putStrLn "post-receive conf-file"
        Just path -> do
            conf <- readFile path
            scotty 7777 $ receiver $ parse conf
