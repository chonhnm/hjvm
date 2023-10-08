module Lib (someFunc) where

import Buffer (runParseClassFile)

someFunc :: IO ()
someFunc = do
  -- parseFile "HelloWorld.class"
  -- parseFileDirect "HelloWorld.class"
  runParseClassFile "HelloWorld.class"
