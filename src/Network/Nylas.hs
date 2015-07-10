{-# LANGUAGE OverloadedStrings #-}

module Network.Nylas where

import           Control.Lens hiding (each)
-- import           Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import qualified Data.Text as T
import           Network.Wreq
import           Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P

import Network.Nylas.Types

-- TODO: user authentication
-- TODO: stream deltas with pipes

deltaUrl :: Namespace -> Url
deltaUrl (Namespace n) = "https://api.nylas.com/n/" <> n <> "/delta"

authenticatedOpts :: AccessToken -> Options -> Options
authenticatedOpts (AccessToken t) = auth ?~ basicAuth (B.pack t) ""

streamDeltas :: AccessToken -> Namespace -> Cursor -> Producer B.ByteString IO ()
streamDeltas t n (Cursor c) = lift mraw >>= PB.fromLazy
   where opts = defaults & authenticatedOpts t
                         & param "cursor" .~ [T.pack c]
         url = deltaUrl n
         mraw = (^. responseBody) <$> (getWith opts url)

messageUrl :: Namespace -> MessageId -> Url
messageUrl (Namespace n) (MessageId i) = "https://api.nylas.com/n/" <> n <> "/messages/" <> i

getMessage :: AccessToken -> Namespace -> MessageId -> IO Message
getMessage t n i = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where opts = defaults & authenticatedOpts t
        url = messageUrl n i

main :: IO ()
main = do
  let token = AccessToken "C8SbrcFVIgnEQi8RdS9beNKnixtEcT"
  let namespace = Namespace "d1z6pzjd1qvalej8bd51abun9"
  let cursor = Cursor "6h6g1xq4d930ja9375mprv6d0"
  let msgId = MessageId "b38i00l4f6qziwl154f57oi1o"

  -- let Cursor c = cursor
  -- let url = deltaUrl namespace
  -- let opts = defaults & authenticatedOpts token
  --                     & param "cursor" .~ [T.pack c]
  -- r <- getWith opts url

  let producer = streamDeltas token namespace cursor
  -- runEffect $ producer >-> _

  msg <- getMessage token namespace msgId
  putStrLn $ show msg
