{-# LANGUAGE OverloadedStrings #-}

module Network.Nylas where

import           Control.Lens hiding (each)
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import qualified Network.Wreq as W
import           Pipes
import qualified Pipes.Aeson.Unchecked as AU
import qualified Pipes.Prelude as P
import           Pipes.HTTP

import Network.Nylas.Types

deltasUrl :: Namespace -> Url
deltasUrl (Namespace n) = "https://api.nylas.com/n/" <> n <> "/delta"

deltaStreamUrl :: Namespace -> Url
deltaStreamUrl (Namespace n) = "https://api.nylas.com/n/" <> n <> "/delta/streaming"

authenticatedOpts :: AccessToken -> W.Options -> W.Options
authenticatedOpts (AccessToken t) = W.auth ?~ W.basicAuth (B.pack t) ""

authenticatedReq :: AccessToken -> Request -> Request
authenticatedReq (AccessToken t) r = applyBasicAuth (B.pack t) (B.pack "") r

consumeDeltas
  :: Manager
  -> AccessToken
  -> Namespace
  -> Cursor
  -> Consumer Delta IO ()
  -> IO ()
consumeDeltas m t n (Cursor c) consumer = do
  req <- parseUrl (deltaStreamUrl n <> "?cursor=" <> c)
  let authdReq = authenticatedReq t req
  withHTTP authdReq m $ \resp -> do
    let body = responseBody resp >-> P.takeWhile (/= "\n")
    let deltas = view AU.decoded body >> return ()
    runEffect $ deltas >-> consumer

messageUrl :: Namespace -> NylasId -> Url
messageUrl (Namespace n) (NylasId i) = "https://api.nylas.com/n/" <> n <> "/messages/" <> i

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

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  let token = AccessToken "C8SbrcFVIgnEQi8RdS9beNKnixtEcT"
  let namespace = Namespace "d1z6pzjd1qvalej8bd51abun9"
  let cursor = Cursor "6h6g1xq4d930ja9375mprv6d0"

  consumeDeltas mgr token namespace cursor (P.map show >-> P.stdoutLn)

  let msgId = NylasId "b38i00l4f6qziwl154f57oi1o"
  putStrLn . show =<< getMessage mgr token namespace msgId
