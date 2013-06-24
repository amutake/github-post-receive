module Main where

import System.Environment
import System.Exit
import Web.Scotty

import Config
import Receiver

type Port = Int

main :: IO ()
main = do
    args <- getArgs
    (port, path) <- checkArgs args
    str <- readFile path
    let result = parseConfig str
    case result of
        Left err -> print err
        Right configs -> scotty port $ receiver configs

checkArgs :: [String] -> IO (Port, FilePath)
checkArgs [port, path] = return (read port, path)
checkArgs _ = putStrLn "post-receive port conf" >> exitFailure
