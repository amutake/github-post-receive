module Github.PostReceive.Server
    ( start
    , app
    ) where

import Control.Applicative ((<$>))
import Data.Aeson (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Network.HTTP.Types (ok200, badRequest400, notFound404, urlDecode, methodPost)
import Network.Wai (requestBody, responseLBS, Application, rawPathInfo, requestMethod)
import Network.Wai.Handler.Warp (run, Port)
import Network.Wai.Logger (withStdoutLogger, ApacheLogger)

import Github.PostReceive.Types (Payload)

start :: Port -> M.Map B.ByteString (Payload -> IO ()) -> IO ()
start port routes = do
    putStrLn startingMessage
    withStdoutLogger $ run port . flip app routes
  where
    startingMessage = concat
        [ "github-post-receive listening on port "
        , show port
        , " with path "
        , show $ M.keys routes
        ]

app :: ApacheLogger -> M.Map B.ByteString (Payload -> IO ()) -> Application
app aplogger routes req respond
    | method == methodPost = do
        bs <- B.drop (length "payload=") <$> requestBody req
        flip (maybe notFound) (M.lookup path routes) $ \cont ->
            flip (maybe badRequest) (decode $ BL.fromStrict $ urlDecode True bs) $ \payload ->
                cont payload >> ok
    | otherwise = notFound
  where
    path = rawPathInfo req
    method = requestMethod req
    res status = aplogger req status Nothing >> respond (responseLBS status [] BL.empty)
    notFound = res notFound404
    badRequest = res badRequest400
    ok = res ok200
