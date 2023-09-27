module Lib(someFunc) where

import Parser
import Buffer
import ClassFile
import Test ( parseReader )

someFunc :: IO ()
someFunc = do
    putStrLn "someFunc"
    -- parseFile "HelloWorld.class"
    -- parseFileDirect "HelloWorld.class"
    parseReader

