module Util where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)

-- TODO:  convert jvm specified utf8 format to text
decodeUtf8Jvm :: B.ByteString -> T.Text
decodeUtf8Jvm = decodeUtf8