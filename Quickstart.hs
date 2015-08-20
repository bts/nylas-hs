{-# LANGUAGE OverloadedStrings #-}

module Quickstart where

import qualified Data.ByteString.Char8 as B
import           Data.Monoid           ((<>))
import           Pipes                 (runEffect, (>->))
import           Pipes.HTTP            (newManager, tlsManagerSettings)
import qualified Pipes.Prelude         as P

import           Network.Nylas

token :: AccessToken
token = AccessToken "_PUT_YOUR_ACCESS_TOKEN_HERE_"

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  let cursor = Nothing
  res <- consumeDeltas mgr token cursor (P.map show >-> P.print)
  case res of
    Left (ParsingError err remainder) -> do
      putStrLn $ "ERROR: " <> show err
      putStrLn "next few items:"
      runEffect $ remainder >-> P.take 3 >-> P.map B.unpack >-> P.stdoutLn
    Left (ConsumerError _) -> error "not possible"
    Right _ -> return ()
