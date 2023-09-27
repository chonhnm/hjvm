module Test (parseReader) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
  ( ReaderT (runReaderT),
    ask,
    local,
  )

parseReader :: IO ()
parseReader = do
  p <- runReaderT parseProfile (Profile "" 0 "")
  print "profile: "
  print p

data Profile = Profile
  { name :: String,
    age :: Int,
    email :: String
  }
  deriving (Show)

parseProfile :: ReaderT Profile IO Profile
parseProfile = do
  name <- parseName
  age <- parseAge
  email <- local (\(Profile _ _ e) -> Profile name age e) parseEmail
  return $ Profile name age email

parseName :: ReaderT Profile IO String
parseName = do
  env <- ask
  lift $ print "name: " >> print env
  let name = "xiaobai"
  local (\p -> p {name = name}) parseAge
  lift $ print "exit name."
  return name

parseAge :: ReaderT Profile IO Int
parseAge = do
  env <- ask
  lift $ print "age: " >> print env
  local (\p -> p {age = 18}) parseEmail
  return 18

parseEmail :: ReaderT Profile IO String
parseEmail = do
  env <- ask
  lift $ print "email: " >> print env
  case env of
    Profile n a _ -> return $ n ++ show a ++ "@qq.com"
