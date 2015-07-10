{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Nylas.Types where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

type Url = String
newtype AccessToken = AccessToken String deriving (Eq, Show)
newtype Cursor = Cursor String deriving (Eq, Show)
newtype Namespace = Namespace String deriving (Eq, Show)
newtype MessageId = MessageId String deriving (Eq, Show, Generic)

data Message
  = Message
  { _messageId :: MessageId
  , _messageSubject :: Text
  } deriving (Eq, Show)

instance FromJSON MessageId

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "id"
            <*> v .: "subject"

  parseJSON _ = empty
