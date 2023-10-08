{-# OPTIONS_GHC -Wno-unused-do-bind #-}

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
  n <- parseName
  ag <- parseAge
  em <- local (\(Profile _ _ e) -> Profile n ag e) parseEmail
  return $ Profile n ag em

parseName :: ReaderT Profile IO String
parseName = do
  env <- ask
  lift $ print "name: " >> print env
  let n = "xiaobai"
  local (\p -> p {name = n}) parseAge
  lift $ print "exit name."
  return n

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
