module Github.PostReceive.Server
    ( start
    ) where

import Data.Aeson (fromJSON, Result (..), json)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (($$), ($=), await, yield)
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Types (ok200, badRequest400)
import Network.Wai (requestBody, responseLBS)
import Network.Wai.Handler.Warp (run, Port)

import Github.PostReceive.Types (Payload)

start :: Port -> (Payload -> IO ()) -> IO ()
start port cont = run port $ \req -> do
    value <- requestBody req $= dropC (length "payload=") $$ sinkParser json
    print value
    case fromJSON value of
        Error err -> putStrLn err >> return (responseLBS badRequest400 [] BL.empty)
        Success payload -> cont payload >> return (responseLBS ok200 [] BL.empty)
  where
    dropC count = do
        mbs <- await
        case mbs of
            Just bs | B.length bs > count -> yield $ B.drop count bs
            Just bs -> dropC $ count - B.length bs
            Nothing -> return ()
