module Network.Nylas.Types where

-- import Data.Text (Text)

type Url = String
newtype AccessToken = AccessToken String deriving (Eq, Show)
newtype Cursor = Cursor String deriving (Eq, Show)
newtype Namespace = Namespace String deriving (Eq, Show)
