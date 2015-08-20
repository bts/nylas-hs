# nylas

An _in-progress_ Haskell client for the [Nylas](https://nylas.com)
[HTTP API](https://www.nylas.com/docs/platform), powered
by [lens](https://hackage.haskell.org/package/lens) and
[pipes](https://hackage.haskell.org/package/pipes). At the moment I've only
implemented support for the endpoints I need.

Pull requests for support of additional endpoints would be greatly appreciated!

## Features

* constant-space stream ingestion of the
  [changes](https://www.nylas.com/docs/platform#deltas) (currently only message
  and thread objects) to an inbox since a given transactional cursor (or the
  beginning of time)
* requesting a message object
* requesting a thread object

## Quickstart

The following example prints out all of the changes to an inbox:

```haskell
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
```

## License (3-clause BSD)

Copyright (c) 2015, Brian Schroeder

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* Neither the name of Brian Schroeder nor the names of other contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
