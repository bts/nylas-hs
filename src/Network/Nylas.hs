{-# LANGUAGE OverloadedStrings #-}

module Network.Nylas where

import           Control.Lens hiding (each)
import qualified Data.ByteString.Char8 as B
-- import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
-- import qualified Data.Text as T
import qualified Network.Wreq as W
import           Pipes
-- import qualified Pipes.Aeson as A
import qualified Pipes.Aeson.Unchecked as AU
-- import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import           Pipes.HTTP

import Network.Nylas.Types

-- TODO: user authentication
-- TODO: stream deltas with pipes

deltasUrl :: Namespace -> Url
deltasUrl (Namespace n) = "https://api.nylas.com/n/" <> n <> "/delta"

deltaStreamUrl :: Namespace -> Url
deltaStreamUrl (Namespace n) = "https://api.nylas.com/n/" <> n <> "/delta/streaming"

authenticatedOpts :: AccessToken -> W.Options -> W.Options
authenticatedOpts (AccessToken t) = W.auth ?~ W.basicAuth (B.pack t) ""

authenticatedReq :: AccessToken -> Request -> Request
authenticatedReq (AccessToken t) r = applyBasicAuth (B.pack t) (B.pack "") r

consumeDeltas
  :: AccessToken
  -> Namespace
  -> Cursor
  -> Consumer Delta IO ()
  -> IO () -- TODO: this should probably return a Cursor
  -- -> IO (Either (A.DecodingError, Producer PB.ByteString IO ()) ())
consumeDeltas t n (Cursor c) consumer = do
  req <- parseUrl (deltaStreamUrl n <> "?cursor=" <> c)
  let authdReq = authenticatedReq t req
  withManager tlsManagerSettings $ \m ->
    withHTTP authdReq m $ \resp -> do
      let body = responseBody resp
      -- TODO: instead of using pipes-aeson's decoded (directly) here, maybe add
      -- a preprocessor that looks for an empty newline and stops
      let deltas = view AU.decoded body >> return ()
      runEffect $ deltas >-> consumer

messageUrl :: Namespace -> NylasId -> Url
messageUrl (Namespace n) (NylasId i) = "https://api.nylas.com/n/" <> n <> "/messages/" <> i

getMessage :: AccessToken -> Namespace -> NylasId -> IO Message
getMessage t n i = (^. W.responseBody) <$> (W.getWith opts url >>= W.asJSON)
  where opts = W.defaults & authenticatedOpts t
        url = messageUrl n i

main :: IO ()
main = do
  let token = AccessToken "C8SbrcFVIgnEQi8RdS9beNKnixtEcT"
  let namespace = Namespace "d1z6pzjd1qvalej8bd51abun9"
  let cursor = Cursor "6h6g1xq4d930ja9375mprv6d0"

  consumeDeltas token namespace cursor (P.map show >-> P.stdoutLn)

  let msgId = NylasId "b38i00l4f6qziwl154f57oi1o"
  msg <- getMessage token namespace msgId
  putStrLn $ show msg
