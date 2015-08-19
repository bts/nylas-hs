{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Nylas.Types where

import Prelude

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as B
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           GHC.Generics (Generic)
import           Pipes (Producer)
import           Pipes.Aeson (DecodingError)

data StreamingError = ParsingError DecodingError (Producer B.ByteString IO ())
                    | ConsumerError Text

instance Show StreamingError where
  show (ParsingError decodingErr _) = "ParsingError (" <> show decodingErr <> ")"
  show (ConsumerError txt) = "ConsumerError " <> show txt

type Url = String

newtype AccessToken = AccessToken Text deriving (Eq, Show)
newtype Cursor = Cursor { _cursorId :: Text } deriving (Eq, Show, Generic)
newtype Namespace = Namespace Text deriving (Eq, Show)
newtype NylasId = NylasId { _nylasId :: Text } deriving (Eq, Show, Generic)

makeLenses ''Cursor
makeLenses ''NylasId

instance FromJSON Cursor where
  parseJSON (String s) = pure $ Cursor s
  parseJSON _ = empty

instance FromJSON NylasId where
  parseJSON (String s) = pure $ NylasId s
  parseJSON _ = empty

data Mailbox
  = Mailbox
  { _mailboxName :: Maybe Text
  , _mailboxEmail :: Text
  } deriving (Eq, Show)

makeLenses ''Mailbox

instance FromJSON Mailbox where
  parseJSON (Object v) =
    Mailbox <$> fmap nonEmpty (v .: "name")
            <*> v .: "email"
    where
      nonEmpty "" = Nothing
      nonEmpty str = Just str
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

newtype MessageTime = MessageTime { _utcTime :: UTCTime } deriving (Eq, Show, Generic)

makeLenses ''MessageTime

instance FromJSON MessageTime where
  parseJSON n = (MessageTime . posixSecondsToUTCTime . fromIntegral) <$>
                (parseJSON n :: Parser Int)

data ReadStatus = MessageRead
                | MessageUnread
                deriving (Eq, Show)

makePrisms ''ReadStatus

data StarStatus = Starred
                | Unstarred
                deriving (Eq, Show)

makePrisms ''StarStatus

instance FromJSON StarStatus where
  parseJSON (Bool True) = pure Starred
  parseJSON (Bool False) = pure Unstarred
  parseJSON _ = empty

data Label
  = Label
  { _labelId :: NylasId
  , _labelName :: Text
  , _labelDisplayName :: Text
  } deriving (Eq, Show)

makeLenses ''Label

instance FromJSON Label where
  parseJSON (Object v) =
    Label <$> v .: "id"
          <*> v .: "name"
          <*> v .: "display_name"
  parseJSON _ = empty

data Folder
  = Folder
  { _folderId :: NylasId
  , _folderName :: Text
  , _folderDisplayName :: Text
  } deriving (Eq, Show)

makeLenses ''Folder

instance FromJSON Folder where
  parseJSON (Object v) =
    Folder <$> v .: "id"
           <*> v .: "name"
           <*> v .: "display_name"
  parseJSON _ = empty

data Message
   = Message
   { _messageId :: NylasId
   , _messageSubject :: Text
   , _messageSenders :: [Mailbox]
   , _messageToRecipients :: [Mailbox]
   , _messageCcRecipients :: [Mailbox]
   , _messageBccRecipients :: [Mailbox]
   -- TODO: add support for reply-tos
   , _messageTime :: MessageTime
   , _messageThreadId :: NylasId
   , _messageFiles :: [File]
   , _messageSnippet :: Text
   , _messageLabels :: Maybe [Label]
   , _messageFolder :: Maybe Folder
   , _messageBody :: Text
   , _messageRead :: ReadStatus
   , _messageStarred :: StarStatus
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
            <*> v .:? "labels"
            <*> v .:? "folder"
            <*> v .: "body"
            <*> fmap fromUnread (v .: "unread")
            <*> v .: "starred"
    where
      fromUnread :: Bool -> ReadStatus
      fromUnread True = MessageUnread
      fromUnread False = MessageRead
  parseJSON _ = empty

data AttachmentsStatus = HasAttachments
                       | NoAttachments
                       deriving (Eq, Show)

makePrisms ''AttachmentsStatus

instance FromJSON AttachmentsStatus where
  parseJSON (Bool True) = pure HasAttachments
  parseJSON (Bool False) = pure NoAttachments
  parseJSON _ = empty

data Thread
  = Thread
  { _threadId :: NylasId
  , _threadSubject :: Text
  , _threadFirstTimestamp :: MessageTime
  , _threadLastTimestamp :: MessageTime
  , _threadParticipants :: [Mailbox]
  , _threadSnippet :: Text
  , _threadLabels :: Maybe [Label]
  , _threadFolders :: Maybe [Folder]
  , _threadMessageIds :: [NylasId]
  , _threadDraftIds :: [NylasId]
  , _threadVersion :: Int
  , _threadStarred :: StarStatus
  , _threadAttachments :: AttachmentsStatus
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
           <*> v .:? "labels"
           <*> v .:? "folders"
           <*> v .: "message_ids"
           <*> v .: "draft_ids"
           <*> v .: "version"
           <*> v .: "starred"
           <*> v .: "has_attachments"
  parseJSON _ = empty

data Change a
  = Create a
  | Modify a
  | Delete
  deriving (Eq, Show)

makePrisms ''Change

data DeltaChange
  = CalendarChange
  | ContactChange
  | EventChange
  | FileChange
  | MessageChange (Change Message)
  | DraftChange
  | ThreadChange (Change Thread)
  | LabelChange
  | FolderChange
  deriving (Eq, Show)

makePrisms ''DeltaChange

data Delta
  = Delta
  { _deltaCursor :: Cursor
  , _deltaObjectId :: NylasId
  , _deltaChange :: DeltaChange
  } deriving (Eq, Show)

makeLenses ''Delta

instance FromJSON Delta where
  parseJSON (Object deltaV) = do
    cursor <- deltaV .: "cursor"
    nyId <- deltaV .: "id"
    (String event) <- deltaV .: "event"
    (String objectType) <- deltaV .: "object"
    mObjV <- deltaV .:? "attributes"
    dc <- case objectType of
      "calendar" -> pure CalendarChange
      "contact" -> pure ContactChange
      "event" -> pure EventChange
      "file" -> pure FileChange
      "message" -> MessageChange <$> parseChange event mObjV
      "draft" -> pure DraftChange
      "thread" -> ThreadChange <$> parseChange event mObjV
      "label" -> pure LabelChange
      "folder" -> pure FolderChange
      _ -> empty
    return $ Delta cursor nyId dc

    where
      parseChange :: FromJSON a => Text -> Maybe Value -> Parser (Change a)
      parseChange "create" (Just objV) = Create <$> parseJSON objV
      parseChange "modify" (Just objV) = Modify <$> parseJSON objV
      parseChange "delete" Nothing = pure Delete
      parseChange _ _ = empty

  parseJSON _ = empty

-- HACK: since pipes-aeson's stream decoding lens is bidirectional, we need to
-- provide this dummy instance:
instance ToJSON Delta where
  toJSON _ = object []
