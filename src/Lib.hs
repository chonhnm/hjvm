module Lib(someFunc) where

import Parser
import Buffer
import ClassFile
import Test ( parseReader )

someFunc :: IO ()
someFunc = do
    -- parseFile "HelloWorld.class"
    -- parseFileDirect "HelloWorld.class"
    runParseClassFile "HelloWorld.class"

