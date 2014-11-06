{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as M
import Github.PostReceive

main :: IO ()
main = start 5678 $ M.fromList [("/", print)]
