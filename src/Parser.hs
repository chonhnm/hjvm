{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}


module Parser where

import Data.ByteString.Lazy as BL
import Data.Functor.Identity (Identity)
import Data.Word ( Word8, Word16, Word32, Word64 )
import Text.Parsec (ParsecT, Stream (..), many1, parse, tokenPrim)
import Text.Parsec.Pos (updatePosChar)
import Numeric (showHex)

type U1 = Word8

type U2 = Word16

type U4 = Word32

type U8 = Word64

-- class (Monad m) => Buffer s m t | s -> t where
--   uncons :: s -> m (Maybe (t, s))

newtype MyString = MyString BL.ByteString

myuncons :: MyString -> Maybe (U1, MyString)
myuncons (MyString bs) = case BL.uncons bs of
  Nothing -> Nothing
  Just (a, b) -> return (a, MyString b)

instance (Monad m) => Stream MyString m U1 where
  uncons = return . myuncons

anyByte :: (Stream s m U1) => ParsecT s u m U1
anyByte = satisfy1 (const True)

satisfy1 :: (Stream s m U1) => (U1 -> Bool) -> ParsecT s u m U1
satisfy1 f =
  tokenPrim
    (\b -> show b)
    (\pos _ _bs -> updatePosChar pos 'a')
    (\b -> if f b then Just b else Nothing)

type Parser = ParsecT MyString () Identity

parseU1 :: Parser U1
parseU1 = satisfy1 (const True)

parseAll :: Parser [U1]
parseAll = many1 parseU1

parseFile :: String -> IO ()
parseFile file = do
  bs <- BL.readFile file
  case parse parseAll "jvm" (MyString bs) of
    Left err -> print err
    Right val -> print $ Prelude.foldr showHex "" val

parseFileDirect :: String -> IO ()
parseFileDirect file = do 
    bs <- BL.readFile file 
    print $ BL.foldr showHex "" bs     