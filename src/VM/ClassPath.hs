module VM.ClassPath where

import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import System.Directory (findFile)
import Util (ClassName, MyErr, AppErr (ClassNotFound))

data ClassPath
  = FSPath FileSystemClassPath
  | ZipPath

newtype FileSystemClassPath = FileSystemClassPath [FilePath]

class ClassPathResolver cp where
  resolve :: cp -> ClassName -> IO (MyErr BL.ByteString)

instance ClassPathResolver FileSystemClassPath where
  resolve fspath name = do
    let FileSystemClassPath dirs = fspath
    let className = convertToBaseName name
    path <- findFile dirs className
    case path of 
        Nothing -> return $ Left $ ClassNotFound className 
        Just x -> Right <$> BL.readFile x 

convertToBaseName :: ClassName -> FilePath
convertToBaseName = T.unpack