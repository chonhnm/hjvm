module Lib(someFunc) where

import Parser
import Buffer
import ClassFile

someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    -- parseFile "HelloWorld.class"
    parseFileDirect "HelloWorld.class"

