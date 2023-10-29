module VM.JavaMain where

import Options.Applicative

data Sample = Sample
  { hello :: String,
    quiet :: Bool,
    enthusiasm :: Int,
    args :: [String]
  }

sample :: Parser Sample
sample =
  Sample
    <$> strOption (long "hello" <> metavar "TARGET" <> help "Target for the greeting")
    <*> switch (long "quiet" <> short 'q' <> help "Whether to be quiet")
    <*> option
      auto
      ( long "enthusiasm" <> short 'e'
          <> help "How enthusiastically to greet"
          <> showDefault
          <> value 1
          <> metavar "INT"
      )
    <*> some (argument str (metavar "ARGS"))

opts :: ParserInfo Sample
opts =
  info
    (sample <**> helper)
    ( fullDesc
        <> progDesc "Print a greeting for TARGET"
        <> header "hello - a test for optparse-applicative"
    )

greet :: Sample -> IO ()
greet (Sample h False n args) = putStrLn $ "Hello, " ++ h ++ replicate n '!' ++ show args 
greet _ = return ()

run :: IO ()
run = greet =<< execParser opts