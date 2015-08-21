{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Network.Nylas.Types where

import           Control.Applicative   (empty, pure, (<*>))
import           Control.Lens          (makeLenses, makePrisms, (^.))
import           Data.Aeson            (FromJSON (parseJSON), ToJSON (toJSON),
                                        Value (String, Object, Bool), object,
                                        (.:), (.:?))
import           Data.Aeson.Types      (Parser)
import           Data.Bool             (Bool (True, False))
import qualified Data.ByteString.Char8 as B
import           Data.Eq               (Eq)
import           Data.Functor          (fmap, (<$>))
import           Data.Int              (Int)
import           Data.Maybe            (Maybe (Nothing, Just))
import           Data.Monoid           ((<>))
import           Data.String           (String)
import           Data.Text             (Text)
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           GHC.Base              (($), (.))
import           GHC.Generics          (Generic)
import           GHC.Real              (fromIntegral)
import           Pipes                 (Producer)
import           Pipes.Aeson           (DecodingError)
import           Prelude               (Show)
import           System.IO             (IO)

-- * Types

type Url = String

-- | The <https://www.nylas.com/docs/platform#authentication access token> from
-- Nylas to access data from a single inbox.
newtype AccessToken = AccessToken Text deriving (Eq, Show)

-- | The unique ID for a Nylas object.
newtype NylasId = NylasId { _nylasId :: Text } deriving (Eq, Show, Generic)

-- ** Nylas Object Types

-- | A tuple of an email address and the optional name accompanying it, for
-- participants in 'Thread's and 'Message's.
data Mailbox
  = Mailbox
  { _mailboxName  :: Maybe Text
  , _mailboxEmail :: Text
  } deriving (Eq, Show)

-- | A file attached to a 'Message'.
data File
   = File
   { _fileId          :: NylasId
   , _fileContentType :: Text
   , _fileName        :: Text
   , _fileSize        :: Int
   , _fileContentId   :: Maybe Text
   } deriving (Eq, Show)

-- | The time a message was sent.
newtype MessageTime = MessageTime { _utcTime :: UTCTime }
                    deriving (Eq, Show, Generic)

-- | Whether a 'Message' has been read in the inbox.
data ReadStatus = MessageRead
                | MessageUnread
                deriving (Eq, Show)

-- | Whether a 'Message' or 'Thread' has been starred in the inbox.
data StarStatus = Starred
                | Unstarred
                deriving (Eq, Show)

-- | A Gmail-style label applied to a 'Message' or 'Thread'.
data Label
  = Label
  { _labelId          :: NylasId
  , _labelName        :: Text
  , _labelDisplayName :: Text
  } deriving (Eq, Show)

-- | An IMAP-style folder in which 'Message's can reside. A 'Thread' can contain
-- 'Messages' spanning multiple folders.
data Folder
  = Folder
  { _folderId          :: NylasId
  , _folderName        :: Text
  , _folderDisplayName :: Text
  } deriving (Eq, Show)

-- | An email message.
data Message
   = Message
   { _messageId            :: NylasId
   , _messageSubject       :: Text
   , _messageSenders       :: [Mailbox]
   , _messageToRecipients  :: [Mailbox]
   , _messageCcRecipients  :: [Mailbox]
   , _messageBccRecipients :: [Mailbox]
   -- TODO: add support for (undocumented?) reply-tos
   , _messageTime          :: MessageTime
   , _messageThreadId      :: NylasId
   , _messageFiles         :: [File]
   , _messageSnippet       :: Text
   , _messageLabels        :: Maybe [Label]
   , _messageFolder        :: Maybe Folder
   , _messageBody          :: Text
   , _messageRead          :: ReadStatus
   , _messageStarred       :: StarStatus
   } deriving (Eq, Show)

-- | Whether a 'Thread' contains any attached 'File's.
data AttachmentsStatus = HasAttachments
                       | NoAttachments
                       deriving (Eq, Show)

-- | A chain of 'Message's.
data Thread
  = Thread
  { _threadId             :: NylasId
  , _threadSubject        :: Text
  , _threadFirstTimestamp :: MessageTime
  , _threadLastTimestamp  :: MessageTime
  , _threadParticipants   :: [Mailbox]
  , _threadSnippet        :: Text
  , _threadLabels         :: Maybe [Label]
  , _threadFolders        :: Maybe [Folder]
  , _threadMessageIds     :: [NylasId]
  , _threadDraftIds       :: [NylasId]
  , _threadVersion        :: Int
  , _threadStarred        :: StarStatus
  , _threadAttachments    :: AttachmentsStatus
  } deriving (Eq, Show)

-- ** Streaming Types

-- | The transactional cursor as of a certain 'Delta'. When requesting a stream
-- of updates, clients should provide the 'Cursor' for the last 'Delta' they
-- successfully consumed.
newtype Cursor = Cursor { _cursorId :: Text } deriving (Eq, Show, Generic)

-- | An error in a streaming pipeline involving 'consumeDeltas'.
data StreamingError = ParsingError DecodingError (Producer B.ByteString IO ())
                    -- ^ An error in parsing JSON
                    | ConsumerError Text
                    -- ^ An error produced by clients while consuming 'Delta's

-- | The change operation wrapping the affected object in a 'Delta'. If the
-- object has been created or modified (as opposed to deleted), the object is
-- nested within the 'Change'.
data Change a
  = Create a
  | Modify a
  | Delete
  deriving (Eq, Show)

-- | The type of modification to the inbox the 'Delta' contains.
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

-- | A change to an inbox with a transactional 'Cursor' to continue processing
-- from this 'Delta' in the future.
data Delta
  = Delta
  { _deltaCursor   :: Cursor
  , _deltaObjectId :: NylasId
  , _deltaChange   :: DeltaChange
  } deriving (Eq, Show)

-- * Lenses

makeLenses ''NylasId
makeLenses ''Mailbox
makeLenses ''File
makeLenses ''MessageTime
makePrisms ''ReadStatus
makePrisms ''StarStatus
makeLenses ''Label
makeLenses ''Folder
makeLenses ''Message
makePrisms ''AttachmentsStatus
makeLenses ''Thread
makeLenses ''Cursor
makePrisms ''Change
makePrisms ''DeltaChange
makeLenses ''Delta

-- * Utilities

-- | A list of the recipients of all types for a 'Message'.
recipients :: Message -> [Mailbox]
recipients m = m ^. messageToRecipients
            <> m ^. messageCcRecipients
            <> m ^. messageBccRecipients

-- Instances

instance FromJSON Mailbox where
  parseJSON (Object v) =
    Mailbox <$> fmap nonEmpty (v .: "name")
            <*> v .: "email"
    where
      nonEmpty "" = Nothing
      nonEmpty str = Just str
  parseJSON _ = empty

instance FromJSON File where
  parseJSON (Object v) =
    File <$> v .: "id"
         <*> v .: "content_type"
         <*> v .: "filename"
         <*> v .: "size"
         <*> v .:? "content_id"
  parseJSON _ = empty

instance FromJSON MessageTime where
  parseJSON n = (MessageTime . posixSecondsToUTCTime . fromIntegral)
            <$> (parseJSON n :: Parser Int)

instance FromJSON StarStatus where
  parseJSON (Bool True) = pure Starred
  parseJSON (Bool False) = pure Unstarred
  parseJSON _ = empty

instance FromJSON Label where
  parseJSON (Object v) =
    Label <$> v .: "id"
          <*> v .: "name"
          <*> v .: "display_name"
  parseJSON _ = empty

instance FromJSON Folder where
  parseJSON (Object v) =
    Folder <$> v .: "id"
           <*> v .: "name"
           <*> v .: "display_name"
  parseJSON _ = empty

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

instance FromJSON AttachmentsStatus where
  parseJSON (Bool True) = pure HasAttachments
  parseJSON (Bool False) = pure NoAttachments
  parseJSON _ = empty

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

instance FromJSON Cursor where
  parseJSON (String s) = pure $ Cursor s
  parseJSON _ = empty

instance FromJSON NylasId where
  parseJSON (String s) = pure $ NylasId s
  parseJSON _ = empty

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
    pure $ Delta cursor nyId dc

    where
      parseChange :: FromJSON a => Text -> Maybe Value -> Parser (Change a)
      parseChange "create" (Just objV) = Create <$> parseJSON objV
      parseChange "modify" (Just objV) = Modify <$> parseJSON objV
      parseChange "delete" Nothing = pure Delete
      parseChange _ _ = empty

  parseJSON _ = empty

-- HACK: since pipes-aeson's stream decoding lens supports bidirectional usage,
-- we need to provide this dummy instance:
instance ToJSON Delta where
  toJSON _ = object []
