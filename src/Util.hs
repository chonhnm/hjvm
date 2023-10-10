module Util
  ( decodeUtf8Jvm,
    module ClassFileConsts,
    AppErr (..),
    ParseError (..),
    RuntimeError (..),
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

data AppErr = PE ParseError 
 | RE RuntimeError 
 | ClassFormatError Text
 deriving (Show)

data ParseError
  = PoolOutOfBoundsException Text
  | PoolUnmatchedType Text
  | NonExausted
  deriving (Show)

data RuntimeError = OutOfBound Text | ServerError Text deriving (Show)

type MyErr = Either AppErr
