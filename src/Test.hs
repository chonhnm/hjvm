{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test where

import ClassFile
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
  ( Reader,
    ReaderT (runReaderT),
    ask,
    asks,
    local,
  )
import Data.Dynamic
import Data.STRef (STRef, modifySTRef', readSTRef, newSTRef)
import Data.Text qualified as T
import Util (MyErr, U2, classFormatErr)

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

hlist :: [Dynamic]
hlist =
  [ toDyn "string",
    toDyn (7 :: Int),
    toDyn 'i'
  ]

hlist0 :: Maybe String
hlist0 = fromDynamic (head hlist)

hlist1 :: Maybe String
hlist1 = fromDynamic (head hlist)

hlist4 :: (Typeable a) => MyErr a
hlist4 = fromDyn (head hlist) $ classFormatErr $ show $ dynTypeRep (head hlist)

hlist5 :: MyErr String
hlist5 = hlist4

data CPInfo2 a = CPInfo2 CPTag a deriving (Functor)

lll :: [CPInfo2 Dynamic]
lll = [CPInfo2 JVM_Constant_Class $ toDyn "dfd"]

l2 :: CPInfo2 String
l2 = CPInfo2 JVM_Constant_Invalid ""

l3 :: CPInfo2 Integer
l3 = CPInfo2 JVM_Constant_Class 1233

lii = [toDyn <$> l2, toDyn <$> l3]

tags :: CPInfo2 a -> CPTag
tags (CPInfo2 tag _) = tag

myUtf8Dyn :: MyErr Dynamic
myUtf8Dyn = return $ toDyn $ ConstUtf8 $ T.pack "hh"

-- ST

data Env s = Env {envCP :: ClassFile, envIdx :: ST s (STRef s Int)}

newtype Myreader a = Myreader (forall s. ReaderT (Env s) (ST s) a)

onRef :: (forall s. ST s (STRef s Int) -> ST s a) -> Myreader a
onRef f = Myreader $ asks envIdx >>= lift . f

readIt :: forall s. ST s (STRef s Int) -> ST s Int
readIt st = do
  ref <- st
  readSTRef ref

addOne :: forall s. ST s (STRef s Int) -> ST s Int
addOne st = do
  ref <- st
  modifySTRef' ref (+ 1)
  readSTRef ref

addInt :: forall s. Int -> ST s (STRef s Int) -> ST s Int
addInt n st = do
  ref <- st
  modifySTRef' ref (+ n)
  readSTRef ref

oneRound :: Myreader Int
oneRound = onRef addOne

twoRound :: Myreader Int
twoRound = onRef $ addInt 2

threeRound :: Myreader Int
threeRound = onRef $ addInt 3

emptyEnv = Env{envIdx = newSTRef 0}

runMyreader :: Myreader a -> a 
runMyreader (Myreader r) =runST $  runReaderT r emptyEnv