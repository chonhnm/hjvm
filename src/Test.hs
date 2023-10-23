{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test where

import ClassFile (CPTag (JVM_Constant_Integer, JVM_Constant_Utf8), ConstInteger (ConstInteger), ConstUtf8 (ConstUtf8))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Kind (Type)
import Data.Text qualified as T
import Data.Typeable (Typeable, cast)

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
  lift $ print ("name: " :: T.Text) >> print env
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

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

hLength :: HList ts -> Int
hLength HNil = 0
hLength (_ :# ts) = 1 + hLength ts

hhead :: HList (t ': ts) -> t
hhead (x :# _) = x

htail :: HList (t ': ts) -> HList ts
htail (_ :# xs) = xs

myList = Just 123 :# True :# HNil

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

----------- CPENTRY------

-- data CPEntry a where
--   ConUtf8 :: ConstUtf8 -> CPEntry ConstUtf8
--   ConInteger :: ConstInteger -> CPEntry ConstInteger

class (Typeable a) => ICPEntry a where
  cpEntryTag :: a -> CPTag

instance ICPEntry ConstUtf8 where
  cpEntryTag _ = JVM_Constant_Utf8

instance ICPEntry ConstInteger where
  cpEntryTag _ = JVM_Constant_Integer

data CPInf = CPInf CPTag CPAny

type CPool = [CPInf]

getU8FromCPool :: CPool -> Int -> Maybe ConstUtf8
getU8FromCPool pool n =
  let (CPInf _ cpany) = pool !! n
   in fromCPAny cpany

data CPAny = forall a. (ICPEntry a) => CPAny a

elimCPAny :: (forall a. (ICPEntry a) => a -> r) -> CPAny -> r
elimCPAny f (CPAny a) = f a

fromCPAny :: (ICPEntry a) => CPAny -> Maybe a
fromCPAny = elimCPAny cast

cpAny1 :: CPAny
cpAny1 = CPAny $ ConstUtf8 "123"

cpAny2 :: CPAny
cpAny2 = CPAny $ ConstInteger 12

getU8 :: Maybe ConstUtf8
getU8 = fromCPAny cpAny1

getInteger :: Maybe ConstInteger
getInteger = fromCPAny cpAny1

matchAny :: CPAny -> CPTag
matchAny = elimCPAny cpEntryTag
