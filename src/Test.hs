{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import qualified Data.Text as T 


parseReader :: IO ()
parseReader = do
  p <- runReaderT parseProfile (Profile "" 0 "")
  putStrLn "profile: "
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
  lift $ print ("name: "::T.Text) >> print env
  let n = "xiaobai"
  local (\p -> p {name = n}) parseAge
  lift $ putStrLn "exit name."
  return n

parseAge :: ReaderT Profile IO Int
parseAge = do
  env <- ask
  lift $ putStrLn "age: " >> print env
  local (\p -> p {age = 18}) parseEmail
  return 18

parseEmail :: ReaderT Profile IO String
parseEmail = do
  env <- ask
  lift $ putStrLn "email: " >> print env
  case env of
    Profile n a _ -> return $ n ++ show a ++ "@qq.com"
