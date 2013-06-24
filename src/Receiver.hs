module Receiver where

import Control.Monad.IO.Class (liftIO)
import Data.String
import System.Cmd
import Web.Scotty

import Conf

receiver :: [Conf] -> ScottyM ()
receiver = mapM_ receiver'
  where
    receiver' (Conf name paths) = do
        let route = fromString $ '/' : name
        post route $ do
            b <- body
            liftIO $ print b
            liftIO $ mapM_ system paths
