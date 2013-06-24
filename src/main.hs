module Main where

import System.Environment
import System.Exit
import Web.Scotty

import Conf
import Receiver

type Port = Int

main :: IO ()
main = do
    args <- getArgs
    (port, path) <- checkArgs args
    conf <- readFile path
    scotty port $ receiver $ parse conf

checkArgs :: [String] -> IO (Port, FilePath)
checkArgs [port, path] = return (read port, path)
checkArgs _ = putStrLn "post-receive port conf" >> exitFailure
