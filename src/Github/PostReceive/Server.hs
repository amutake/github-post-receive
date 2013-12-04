module Github.PostReceive.Server
    ( start
    ) where

import Control.Applicative ((<$>))
import Data.Aeson (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import Network.HTTP.Types (ok200, badRequest400, urlDecode)
import Network.Wai (requestBody, responseLBS)
import Network.Wai.Handler.Warp (run, Port)

import Github.PostReceive.Types (Payload)

start :: Port -> (Payload -> IO ()) -> IO ()
start port cont = run port $ \req -> do
    bs <- B.drop (length "payload=") . B.concat <$> (requestBody req $$ CL.consume)
    case decode $ BL.fromStrict $ urlDecode True bs of
        Nothing -> return (responseLBS badRequest400 [] BL.empty)
        Just payload -> cont payload >> return (responseLBS ok200 [] BL.empty)
