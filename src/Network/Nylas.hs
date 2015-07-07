{-# LANGUAGE OverloadedStrings #-}

module Network.Nylas where

import           Control.Lens
-- import Pipes
-- import Pipes.Prelude as P
-- import           Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid
import qualified Data.Text as T
-- import Data.Text (pack)
import           Network.Wreq

import Network.Nylas.Types

-- TODO: user authentication
-- TODO: stream deltas with pipes

deltaUrl :: Namespace -> Url
deltaUrl (Namespace n) = "https://api.nylas.com/n/" <> n <> "/delta"

authenticatedOpts :: AccessToken -> Options -> Options
authenticatedOpts (AccessToken t) = auth ?~ basicAuth (BS.pack t) ""

getDeltas :: AccessToken -> Namespace -> Cursor -> IO (Response BL.ByteString)
getDeltas t n (Cursor c) = getWith opts url
   where opts = defaults & authenticatedOpts t
                         & param "cursor" .~ [T.pack c]
         url = deltaUrl n

main :: IO ()
main = do
  let token = AccessToken "C8SbrcFVIgnEQi8RdS9beNKnixtEcT"
  let namespace = Namespace "d1z6pzjd1qvalej8bd51abun9"
  let cursor = Cursor "6h6g1xq4d930ja9375mprv6d0"

  -- let Cursor c = cursor
  -- let url = deltaUrl namespace
  -- let opts = defaults & authenticatedOpts token
  --                     & param "cursor" .~ [T.pack c]
  -- r <- getWith opts url

  r <- getDeltas token namespace cursor

  putStrLn $ show $ r ^. responseBody
