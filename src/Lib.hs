module Lib (someFunc) where

import VM.JavaMain (run)

someFunc :: IO ()
someFunc = do
  -- parseFile "HelloWorld.class"
  -- parseFileDirect "HelloWorld.class"
  -- runParseClassFile "HelloWorld.class"
  run
