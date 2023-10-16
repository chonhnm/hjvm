module Util
  ( decodeUtf8Jvm,
    module ClassFileConsts,
    AppErr (..),
    CheckedError (..),
    MyErr,
  )
where

import ClassFileConsts
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

-- TODO:  convert jvm specified utf8 format to text
decodeUtf8Jvm :: B.ByteString -> Text
decodeUtf8Jvm = decodeUtf8

data AppErr
  = PE CheckedError
  | ClassFormatError String
  | UnknownErr String
  deriving (Show)

data CheckedError
  = PoolOutOfBoundsException String
  | PoolUnmatchedType String
  | NonExausted
  deriving (Show)


type MyErr = Either AppErr
