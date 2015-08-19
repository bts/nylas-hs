{-# LANGUAGE OverloadedStrings #-}

module Network.Nylas.Client
       ( consumeDeltas
       , getMessage
       , getThread
       ) where

import           Prelude

import           Control.Lens          hiding (each)
import qualified Data.ByteString.Char8 as B
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import qualified Network.Wreq          as W
import           Pipes                 (Consumer, Producer, runEffect, (>->))
import           Pipes.Aeson           (DecodingError)
import qualified Pipes.Aeson.Unchecked as AU
import           Pipes.HTTP            (Manager, Request, applyBasicAuth,
                                        parseUrl, responseBody, withHTTP)
import qualified Pipes.Prelude         as P

import           Network.Nylas.Types

deltaStreamUrl :: Namespace -> Url
deltaStreamUrl (Namespace n) =
  T.unpack $ "https://api.nylas.com/n/" <> n <> "/delta/streaming"

authenticatedOpts :: AccessToken -> W.Options -> W.Options
authenticatedOpts (AccessToken t) = W.auth ?~ W.basicAuth (E.encodeUtf8 t) ""

authenticatedReq :: AccessToken -> Request -> Request
authenticatedReq (AccessToken t) =
  applyBasicAuth (E.encodeUtf8 t) (E.encodeUtf8 "")

consumeDeltas
  :: Manager
  -> AccessToken
  -> Namespace
  -> Maybe Cursor
  -> Consumer Delta IO (Either StreamingError ())
  -> IO (Either StreamingError ())
consumeDeltas m t n mCursor consumer = do
  let c = maybe "0" _cursorId mCursor
  req <- parseUrl (deltaStreamUrl n <> "?cursor=" <> T.unpack c)
  let authdReq = authenticatedReq t req
  withHTTP authdReq m $ \resp -> do
    let body = responseBody resp >-> P.takeWhile (/= "\n")
        deltas = wrapError <$> view AU.decoded body
    runEffect $ deltas >-> consumer

  where
    wrapError :: Either (DecodingError, Producer B.ByteString IO ()) ()
              -> Either StreamingError ()
    wrapError (Left (err, leftovers)) = Left $ ParsingError err leftovers
    wrapError _ = Right ()

messageUrl :: Namespace -> NylasId -> Url
messageUrl (Namespace n) (NylasId i) =
  T.unpack $ "https://api.nylas.com/n/" <> n <> "/messages/" <> i

getMessage
  :: Manager
  -> AccessToken
  -> Namespace
  -> NylasId
  -> IO Message
getMessage mgr t n i = (^. W.responseBody) <$> (W.asJSON =<< W.getWith opts url)
  where opts = W.defaults & authenticatedOpts t
                          & W.manager .~ Right mgr
        url = messageUrl n i

threadUrl :: Namespace -> NylasId -> Url
threadUrl (Namespace n) (NylasId i) =
  T.unpack $ "https://api.nylas.com/n/" <> n <> "/threads/" <> i

getThread
  :: Manager
  -> AccessToken
  -> Namespace
  -> NylasId
  -> IO Thread
getThread mgr t n i = (^. W.responseBody) <$> (W.asJSON =<< W.getWith opts url)
  where opts = W.defaults & authenticatedOpts t
                          & W.manager .~ Right mgr
        url = threadUrl n i
