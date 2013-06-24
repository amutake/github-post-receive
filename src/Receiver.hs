{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Receiver where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.String
import System.Cmd
import Web.Scotty

import Conf
import Message

receiver :: [Conf] -> ScottyM ()
receiver confs = do
    get "/" $ html "<h1>Post Recieve Server</h1>"
    mapM_ receiver' confs
  where
    receiver' (Conf name paths) = do
        let route = fromString $ '/' : name
        post route $ do
            str <- body
            let result = decode $ B.drop 8 str
            case result of
                Nothing -> liftIO $ print str
                Just (msg :: Message) -> do
                    liftIO $ print msg
                    liftIO $ mapM_ system paths
