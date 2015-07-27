{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Nylas.Types where

import Prelude

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)

type Url = String

newtype AccessToken = AccessToken Text deriving (Eq, Show)
newtype Cursor = Cursor Text deriving (Eq, Show, Generic)
newtype Namespace = Namespace Text deriving (Eq, Show)
newtype NylasId = NylasId Text deriving (Eq, Show, Generic)

makeLenses ''AccessToken
makeLenses ''Cursor
makeLenses ''Namespace
makeLenses ''NylasId

instance FromJSON Cursor
instance FromJSON NylasId

data Mailbox
  = Mailbox
  { _mailboxName :: Text -- TODO: make this Maybe Text
  , _mailboxEmail :: Text
  } deriving (Eq, Show)

makeLenses ''Mailbox

instance FromJSON Mailbox where
  parseJSON (Object v) =
    Mailbox <$> v .: "name"
            <*> v .: "email"
  parseJSON _ = empty

data File
   = File
   { _fileId :: NylasId
   , _fileContentType :: Text
   , _fileName :: Text
   , _fileSize :: Int
   , _fileContentId :: Maybe Text
   } deriving (Eq, Show)

makeLenses ''File

instance FromJSON File where
  parseJSON (Object v) =
    File <$> v .: "id"
         <*> v .: "content_type"
         <*> v .: "filename"
         <*> v .: "size"
         <*> v .:? "content_id"
  parseJSON _ = empty

newtype MessageTime = MessageTime UTCTime deriving (Eq, Show, Generic)

makeLenses ''MessageTime

instance FromJSON MessageTime where
  parseJSON n = (MessageTime . posixSecondsToUTCTime . fromIntegral) <$>
                (parseJSON n :: Parser Int)

newtype MessageUnread = MessageUnread Bool deriving (Eq, Show, Generic)

makeLenses ''MessageUnread

instance FromJSON MessageUnread

data Message
   = Message
   { _messageId :: NylasId
   , _messageSubject :: Text
   , _messageSenders :: [Mailbox]
   , _messageToRecipients :: [Mailbox]
   , _messageCcRecipients :: [Mailbox]
   , _messageBccRecipients :: [Mailbox]
   , _messageTime :: MessageTime
   , _messageThreadId :: NylasId
   , _messageFiles :: [File]
   , _messageSnippet :: Text
   , _messageBody :: Text
   , _messageUnread :: MessageUnread
   } deriving (Eq, Show)

makeLenses ''Message

recipients :: Message -> [Mailbox]
recipients m = m^.messageToRecipients <>
               m^.messageCcRecipients <>
               m^.messageBccRecipients

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "id"
            <*> v .: "subject"
            <*> v .: "from"
            <*> v .: "to"
            <*> v .: "cc"
            <*> v .: "bcc"
            <*> v .: "date"
            <*> v .: "thread_id"
            <*> v .: "files"
            <*> v .: "snippet"
            <*> v .: "body"
            <*> v .: "unread"
  parseJSON _ = empty

data Tag
  = Tag
  { _tagId :: NylasId
  , _tagName :: Text
  } deriving (Eq, Show)

makeLenses ''Tag

instance FromJSON Tag where
  parseJSON (Object v) =
    Tag <$> v .: "id"
        <*> v .: "name"
  parseJSON _ = empty

data Thread
  = Thread
  { _threadId :: NylasId
  , _threadSubject :: Text
  , _threadFirstTimestamp :: MessageTime
  , _threadLastTimestamp :: MessageTime
  , _threadParticipants :: [Mailbox]
  , _threadSnippet :: Text
  , _threadTags :: [Tag]
  , _threadMessageIds :: [NylasId]
  , _threadDraftIds :: [NylasId]
  , _threadVersion :: Int
  } deriving (Eq, Show)

makeLenses ''Thread

instance FromJSON Thread where
  parseJSON (Object v) =
    Thread <$> v .: "id"
           <*> v .: "subject"
           <*> v .: "first_message_timestamp"
           <*> v .: "last_message_timestamp"
           <*> v .: "participants"
           <*> v .: "snippet"
           <*> v .: "tags"
           <*> v .: "message_ids"
           <*> v .: "draft_ids"
           <*> v .: "version"
  parseJSON _ = empty

data DeltaObject
  = DeltaCalendar
  | DeltaContact
  | DeltaEvent
  | DeltaFile
  | DeltaMessage Message
  | DeltaTag
  | DeltaThread Thread
  deriving (Eq, Show)

makePrisms ''DeltaObject

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
      "thread" -> DeltaThread <$> (parseJSON o)
      _ -> empty
  parseJSON _ = empty

data DeltaOperation
  = Create
  | Modify
  | Delete
  deriving (Eq, Show)

makePrisms ''DeltaOperation

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

makeLenses ''Delta

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
  toJSON _ = object []
instance ToJSON Delta where
  toJSON _ = object []
