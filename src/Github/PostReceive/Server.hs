{-# LANGUAGE OverloadedStrings #-}

module Github.PostReceive.Server
    ( start
    ) where

import Data.Aeson (fromJSON, Result (..), json)
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Types (ok200, badRequest400)
import Network.Wai (requestBody, responseLBS)
import Network.Wai.Handler.Warp (run, Port)

import Github.PostReceive.Types (Payload)

start :: Port -> (Payload -> IO ()) -> IO ()
start port cont = run port $ \req -> do
    value <- requestBody req $$ sinkParser json
    print value
    case fromJSON value of
        Error err -> putStrLn err >> return (responseLBS badRequest400 [] "")
        Success payload -> cont payload >> return (responseLBS ok200 [] "")
