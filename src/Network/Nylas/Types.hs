{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Nylas.Types where

import Control.Applicative
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

type Url = String
newtype AccessToken = AccessToken String deriving (Eq, Show)
newtype Cursor = Cursor String deriving (Eq, Show, Generic)
newtype Namespace = Namespace String deriving (Eq, Show)
newtype NylasId = NylasId String deriving (Eq, Show, Generic)

instance FromJSON Cursor
instance FromJSON NylasId

data Message
   = Message
   { _messageId :: NylasId
   , _messageSubject :: Text
   } deriving (Eq, Show)

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "id"
            <*> v .: "subject"
            -- TODO: more
  parseJSON _ = empty

data DeltaObject
  = DeltaCalendar
  | DeltaContact
  | DeltaEvent
  | DeltaFile
  | DeltaMessage Message
  | DeltaTag
  | DeltaThread
  deriving (Eq, Show)

instance FromJSON DeltaObject where
  parseJSON o@(Object v) = do
    (String objectType) <- v .: "object"
    case objectType of
      "calendar" -> pure DeltaCalendar
      "contact" -> pure DeltaContact
      "event" -> pure DeltaEvent
      "file" -> pure DeltaFile
      "message" -> DeltaMessage <$> (parseJSON o)
      "tag" -> pure DeltaTag
      "thread" -> pure DeltaThread
      _ -> empty
  parseJSON _ = empty

data DeltaOperation
  = Create
  | Modify
  | Delete
  deriving (Eq, Show)

instance FromJSON DeltaOperation where
  parseJSON (String "create") = pure Create
  parseJSON (String "modify") = pure Modify
  parseJSON (String "delete") = pure Delete
  parseJSON _ = empty

data Delta
  = Delta
  { _deltaCursor :: Cursor
  , _deltaOperation :: DeltaOperation
  , _deltaObjectId :: NylasId
  , _deltaObject :: DeltaObject
  } deriving (Eq, Show)

instance FromJSON Delta where
  parseJSON (Object v) =
    Delta <$> v .: "cursor"
          <*> v .: "event"
          <*> v .: "id"
          <*> v .: "attributes"
  parseJSON _ = empty

--
-- TODO: does pipes-aeson *really* require these instances for stream decoding?
--
instance ToJSON NylasId
instance ToJSON Message where
  toJSON (Message {_messageId = i}) = object ["id" .= i]
