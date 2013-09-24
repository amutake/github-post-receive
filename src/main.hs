module Main where

import System.Environment
import System.Exit
import Web.Scotty

import Config
import Receiver
import Util

type Port = Int

main :: IO ()
main = do
    args <- getArgs
    (port, path) <- checkArgs args
    str <- readFile path
    let result = parseConfig str
    case result of
        Left err -> print err
        Right configs -> do
          logMessage $ "github-post-receive listening on port " ++ show port ++ " with config " ++ path
          scotty port $ receiver configs

checkArgs :: [String] -> IO (Port, FilePath)
checkArgs [port, path] = return (read port, path)
checkArgs _ = putStrLn "post-receive port conf" >> exitFailure
