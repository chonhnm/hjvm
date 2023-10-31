{-# OPTIONS_GHC -Wno-missing-fields #-}

module VM.JavaMain where

import Data.Maybe (maybeToList)
import Data.Text qualified as T
import Options.Applicative
import System.Directory (canonicalizePath, getCurrentDirectory)

data Options = Options
  { classpath :: Maybe String,
    classpathes :: Maybe String,
    jarfile :: Maybe String,
    javargs :: [String]
  }
  deriving (Show)

data JavaRunType = RunJar String | RunMainClass String deriving (Show)

data JavaOptions = JavaOptions
  { opRunType :: JavaRunType,
    opClasspathes :: [String],
    opJavargs :: [String]
  }
  deriving (Show)

optionInfo :: ParserInfo Options
optionInfo =
  info
    (options <**> helper)
    ( fullDesc
        <> progDesc "run java app."
    )

options :: Parser Options
options =
  Options
    <$> optional
      ( strOption $
          long "classpath"
            <> long "cp"
            <> metavar "CLASSPATH"
            <> help "class search path of directories and zip/jar files"
      )
    <*> optional
      ( strOption $
          long "class-path"
            <> metavar "CLASS-PATH"
            <> help "A : separated class search path of directories and zip/jar files"
      )
    <*> optional
      ( strOption $
          long "jar"
            <> metavar "jarfile"
      )
    <*> many (argument str (metavar "ARGS"))

parseOptions :: IO JavaOptions
parseOptions = do
  ops <- execParser optionInfo
  let cp = maybeToList $ classpath ops
  let cps = concatMap (T.splitOn ":" . T.pack) (maybeToList $ classpathes ops)
  let cpss = cp ++ map T.unpack cps
  canoPathes <- do
    ps <- mapM canonicalizePath cpss
    if null ps
      then do
        curp <- getCurrentDirectory
        return [curp]
      else return ps
  let jp = JavaOptions {opClasspathes = canoPathes}
  let args = javargs ops
  case jarfile ops of
    Just x -> return jp {opRunType = RunJar x, opJavargs = args}
    Nothing -> case args of
      [] -> error "specify jarfile or mainclass."
      (x : xs) -> return jp {opRunType = RunMainClass x, opJavargs = xs}

run :: IO ()
run = do
  ops <- parseOptions
  print ops