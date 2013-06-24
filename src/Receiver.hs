{-# LANGUAGE OverloadedStrings #-}

module Receiver where

import Control.Monad.IO.Class (liftIO)
import Data.String
import System.Cmd
import Web.Scotty

import Conf

receiver :: [Conf] -> ScottyM ()
receiver confs = do
    get "/" $ html "<h1>Post Recieve Server</h1>"
    mapM_ receiver' confs
  where
    receiver' (Conf name paths) = do
        let route = fromString $ '/' : name
        post route $ do
            b <- body
            liftIO $ print b
            liftIO $ mapM_ system paths
