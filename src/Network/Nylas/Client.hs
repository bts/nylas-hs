{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Nylas.Client
       ( consumeDeltas
       , getMessage
       , getThread
       ) where

import           Control.Lens          (view, (&), (.~), (?~), (^.))
import           Control.Monad         ((=<<))
import qualified Data.ByteString.Char8 as B
import           Data.Either           (Either (Left, Right))
import           Data.Eq               ((/=))
import           Data.Functor          ((<$>))
import           Data.Maybe            (Maybe, maybe)
import           Data.Monoid           ((<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as E
import           GHC.Base              (($))
import qualified Network.Wreq          as W
import           Pipes                 (Consumer, Producer, runEffect, (>->))
import           Pipes.Aeson           (DecodingError)
import qualified Pipes.Aeson.Unchecked as AU
import           Pipes.HTTP            (Manager, Request, applyBasicAuth,
                                        parseUrl, responseBody, withHTTP)
import qualified Pipes.Prelude         as P
import           System.IO             (IO)

import           Network.Nylas.Types

authenticatedOpts :: AccessToken -> W.Options -> W.Options
authenticatedOpts (AccessToken t) = W.auth ?~ W.basicAuth (E.encodeUtf8 t) ""

authenticatedReq :: AccessToken -> Request -> Request
authenticatedReq (AccessToken t) =
  applyBasicAuth (E.encodeUtf8 t) (E.encodeUtf8 "")

deltaStreamUrl :: Maybe Cursor -> Url
deltaStreamUrl mCursor =
  T.unpack $ "https://api.nylas.com/delta/streaming?cursor="
          <> maybe "0" _cursorId mCursor

consumeDeltas
  :: Manager
  -> AccessToken
  -> Maybe Cursor
  -> Consumer Delta IO (Either StreamingError ())
  -> IO (Either StreamingError ())
consumeDeltas m t mCursor consumer = do
  req <- parseUrl $ deltaStreamUrl mCursor
  withHTTP (authenticatedReq t req) m $ \resp -> do
    let body = responseBody resp >-> P.takeWhile (/= "\n")
        deltas = wrapError <$> view AU.decoded body
    runEffect $ deltas >-> consumer

  where
    wrapError :: Either (DecodingError, Producer B.ByteString IO ()) ()
              -> Either StreamingError ()
    wrapError (Left (err, leftovers)) = Left $ ParsingError err leftovers
    wrapError _ = Right ()

messageUrl :: NylasId -> Url
messageUrl (NylasId i) = T.unpack $ "https://api.nylas.com/messages/" <> i

getMessage :: Manager -> AccessToken -> NylasId -> IO Message
getMessage mgr t i = (^. W.responseBody) <$> (W.asJSON =<< W.getWith opts url)
  where opts = W.defaults & authenticatedOpts t
                          & W.manager .~ Right mgr
        url = messageUrl i

threadUrl :: NylasId -> Url
threadUrl (NylasId i) = T.unpack $ "https://api.nylas.com/threads/" <> i

getThread :: Manager -> AccessToken -> NylasId -> IO Thread
getThread mgr t i = (^. W.responseBody) <$> (W.asJSON =<< W.getWith opts url)
  where opts = W.defaults & authenticatedOpts t
                          & W.manager .~ Right mgr
        url = threadUrl i
