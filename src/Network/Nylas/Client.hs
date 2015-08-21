{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Nylas.Client
       ( -- * Streaming Delta Consumption
         consumeDeltas
         -- * Requesting Individual Objects
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

-- | Consume 'Delta's for the inbox associated with the provided 'AccessToken'
-- since the optional 'Cursor' from Nylas' transactional
-- <https://www.nylas.com/docs/platform#streaming_delta_updates Streaming Delta Updates endpoint>.
--
-- Clients should keep track of the 'Cursor' from the latest 'Delta' consumed to
-- resume consumption in the future.
--
-- Any errors encountered during consumption (e.g. while writing to a database)
-- should be surfaced using the 'ConsumerError' value constructor of
-- 'StreamingError'.
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

-- | Fetch the 'Message' identified by 'NylasId' from the account associated
-- with 'AccessToken'.
getMessage :: Manager -> AccessToken -> NylasId -> IO Message
getMessage mgr t i = (^. W.responseBody) <$> (W.asJSON =<< W.getWith opts url)
  where opts = W.defaults & authenticatedOpts t
                          & W.manager .~ Right mgr
        url = messageUrl i

threadUrl :: NylasId -> Url
threadUrl (NylasId i) = T.unpack $ "https://api.nylas.com/threads/" <> i

-- | Fetch the 'Thread' identified by 'NylasId' from the account associated with
-- 'AccessToken'.
getThread :: Manager -> AccessToken -> NylasId -> IO Thread
getThread mgr t i = (^. W.responseBody) <$> (W.asJSON =<< W.getWith opts url)
  where opts = W.defaults & authenticatedOpts t
                          & W.manager .~ Right mgr
        url = threadUrl i
