{-# LANGUAGE DeriveDataTypeable #-}

module Test where

import ClassFile (ConstantFloat, ConstantInteger, ConstantPoolInfo (cpInfo))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
  ( ReaderT (runReaderT),
    ask,
    local,
  )
import Data.Data
import Data.Data (constrFields)
import Data.Int
import Data.List (insert)
import Data.Text qualified as T
import Data.Typeable (TypeRep, Typeable, cast, typeOf)
import Text.Printf

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

data CPInfo
  = CU ConstUtf8
  | CI ConstInteger
  | CF ConstFloat
  | CL ConstLong
  | CD ConstDouble
  deriving (Typeable, Data, Show)

newtype ConstUtf8 = ConstUtf8 String deriving (Data, Show)

newtype ConstInteger = ConstInteger Int32 deriving (Data, Show)

newtype ConstFloat = ConstFloat Float deriving (Data, Show)

newtype ConstLong = ConstLong Int64 deriving (Data, Show)

newtype ConstDouble = ConstDouble Double deriving (Data, Show)

a1 = CU $ ConstUtf8 "agc"

a2 = CI $ ConstInteger 12

a3 = CF $ ConstFloat 2.1

readConstUtf8 :: CPInfo -> ConstUtf8
readConstUtf8 cp = let CU x = cp in x

readConstInteger :: CPInfo -> ConstInteger
readConstInteger cp = let CI x = cp in x

readConstFloat :: CPInfo -> ConstFloat
readConstFloat cp = let CF x = cp in x

checkType :: TypeRep -> CPInfo -> CPInfo
checkType re cp =
  if typeOf cp == re
    then cp
    else error "Unmatched type."

readCU :: CPInfo -> CPInfo
readCU = checkType $ typeOf CU

readCI :: CPInfo -> CPInfo
readCI = checkType $ typeOf CI

readCF :: CPInfo -> CPInfo
readCF = checkType $ typeOf CF

class CP a

instance CP ConstUtf8

instance CP ConstInteger

instance CP ConstFloat

type GoID = Int

type Dt = Float

data GameObject = GameObject {goID :: GoID, render :: IO (), update :: Dt -> GameObject}

type Level = Int

type Elapsed = Float

data Apple = Apple {level :: Level, elapsed :: Elapsed}

updateApple :: Apple -> Dt -> Apple
updateApple (Apple level elapsed) dt = let elapsed' = elapsed + dt in if elapsed' >= 5.0 then Apple (level + 1) (elapsed' - 5.0) else Apple level elapsed'

apple :: Apple -> GoID -> GameObject
apple a goid = GameObject {goID = goid, render = printf "apple: id %d, level %d\n" goid (level a), update = flip apple goid . updateApple a}

banana :: GoID -> GameObject
banana goid = GameObject {goID = goid, render = printf "banana: id %d\n" goid, update = const $ banana goid}

newtype CF a = CFF {unwarp :: a}

u8 = CFF ConstUtf8

jj :: String -> ConstUtf8
jj = unwarp u8

kk = typeRep ConstDouble

data CPL = A ConstantInteger | B ConstantFloat

readA :: CPL -> ConstantInteger
readA cp = let A x = cp in x

class CPP cp where
  unwarp1 :: cp a -> a

instance CPP ConstPool where
  unwarp1 (ConstPool a) = a

newtype ConstPool a = ConstPool {unwrap :: a}

cpUtf8 :: ConstPool ConstUtf8
cpUtf8 = ConstPool $ ConstUtf8 "jvm"

cpInt :: ConstPool ConstInteger
cpInt = ConstPool $ ConstInteger 12

utf8Val :: ConstUtf8
utf8Val = unwrap cpUtf8

intVal :: ConstInteger
intVal = unwrap cpInt
