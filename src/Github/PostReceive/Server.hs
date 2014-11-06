{-# LANGUAGE OverloadedStrings #-}

module Github.PostReceive.Server
    ( start
    , app
    ) where

import Control.Applicative ((<$>))
import Data.Aeson (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Network.HTTP.Types
    ( ok200, badRequest400, notFound404, internalServerError500
    , urlDecode, methodPost, hContentType
    )
import Network.Wai
    ( strictRequestBody, responseLBS, Application
    , rawPathInfo, requestMethod, requestHeaders
    )
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
    | method == methodPost = flip (maybe notFound) (M.lookup path routes) $ \cont ->
        case contentType of
            Just "application/json" -> jsonCase cont
            Just "application/x-www-form-urlencoded" -> formCase cont
            _ -> badRequest
    | otherwise = notFound
  where
    path = rawPathInfo req
    method = requestMethod req
    contentType = lookup hContentType $ requestHeaders req
    res status = aplogger req status Nothing >> respond (responseLBS status [] BL.empty)
    notFound = res notFound404
    badRequest = res badRequest400
    internalError = res internalServerError500
    ok = res ok200
    jsonCase cont = do
        bs <- strictRequestBody req
        flip (maybe internalError) (decode bs) $ \payload -> cont payload >> ok
    formCase cont = do
        bs <- BL.drop (BL.length "payload=") <$> strictRequestBody req
        flip (maybe internalError) (decode $ BL.fromStrict $ urlDecode True $ BL.toStrict bs) $ \payload ->
            cont payload >> ok
