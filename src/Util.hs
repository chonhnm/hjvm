module Util
  ( decodeUtf8Jvm,
    module ClassFileConsts,
    AppErr (..),
    CheckedError (..),
    MyErr,
    classFormatErr,
    bitLength,
    digitLength,
    ClassName,
  )
where

import ClassFileConsts
import Data.Bits (Bits (shiftR))
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)

-- TODO:  convert jvm specified utf8 format to text
decodeUtf8Jvm :: B.ByteString -> Text
decodeUtf8Jvm = decodeUtf8

data AppErr
  = PE CheckedError
  | ClassFormatError String
  | ClassNotFound String
  | UnknownErr String
  deriving (Show)

data CheckedError
  = PoolOutOfBoundsException String
  | PoolUnmatchedType String
  | NonExausted
  deriving (Show)

type MyErr = Either AppErr

classFormatErr :: String -> Either AppErr a
classFormatErr str = Left $ ClassFormatError str

bitLen_ :: (Integral a, Bits a) => a -> Int
bitLen_ 0 = 0
bitLen_ n = 1 + bitLength (shiftR n 1)

bitLength :: (Integral a, Bits a) => a -> Int
bitLength n
  | n == 0 = 1
  | n < 0 = 1 + bitLen_ (abs n)
  | otherwise = bitLen_ n

digitLength :: (Integral a) => a -> Int
digitLength n
  | n == 0 = 1
  | n < 0 = 1 + digitLength_ (abs n)
  | otherwise = digitLength_ n

digitLength_ :: (Integral a) => a -> Int
digitLength_ 0 = 0
digitLength_ n = 1 + digitLength_ (n `div` 10)

type ClassName = T.Text