module Main where

import Github.PostReceive.Server

main :: IO ()
main = start 5678 print
