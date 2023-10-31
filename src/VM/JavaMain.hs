module VM.JavaMain where

import Options.Applicative

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
  let cp = classpath ops 
  let cps = classpathes ops 

  let jp = JavaOptions {opClasspathes = []}
  let args = javargs ops 
  case jarfile ops of 
    Just x -> return  jp {opRunType=RunJar x, opJavargs=args}
    Nothing -> case args of 
      [] -> error "specify jarfile or mainclass."
      (x:xs) -> return jp{opRunType=RunMainClass x, opJavargs=xs} 


run :: IO ()
run = do
  ops <- parseOptions
  print ops