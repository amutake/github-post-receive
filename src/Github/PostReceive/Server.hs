module Github.PostReceive.Server
    ( start
    ) where

import Control.Applicative ((<$>))
import Data.Aeson (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Types (ok200, badRequest400, urlDecode)
import Network.Wai (requestBody, responseLBS)
import Network.Wai.Handler.Warp (run, Port)

import Github.PostReceive.Types (Payload)

start :: Port -> (Payload -> IO ()) -> IO ()
start port cont = run port $ \req respond -> do
    bs <- B.drop (length "payload=") <$> requestBody req
    case decode $ BL.fromStrict $ urlDecode True bs of
        Nothing -> respond (responseLBS badRequest400 [] BL.empty)
        Just payload -> cont payload >> respond (responseLBS ok200 [] BL.empty)
