-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Test.Test_Exists where

import ClassFile (CPTag (..), ConstInteger (ConstInteger), ConstUtf8 (ConstUtf8))
import Data.Typeable (Typeable, cast)

class (Typeable a, HasCPEntryTag a) => HasCPEntry a

instance HasCPEntry ConstUtf8

instance HasCPEntry ConstInteger

class HasCPEntryTag a where
  cpEntryTag :: a -> CPTag

instance HasCPEntryTag ConstUtf8 where
  cpEntryTag _ = JVM_Constant_Utf8

instance HasCPEntryTag ConstInteger where
  cpEntryTag _ = JVM_Constant_Integer

data CPAny = forall a. (HasCPEntry a) => CPAny a

elimCPAny :: (forall a. (HasCPEntry a) => (a -> r)) -> CPAny -> r
elimCPAny f (CPAny a) = f a

castCPAny :: (Typeable a) => CPAny -> Maybe a
castCPAny = elimCPAny cast

tagCPAny :: CPAny -> CPTag
tagCPAny = elimCPAny cpEntryTag

data Any = forall a. (Show a, Typeable a) => Any a

entries :: [Any]
entries = [Any $ ConstUtf8 "hello", Any $ ConstInteger 123]

elimAny :: (forall a. (Show a, Typeable a) => (a -> r)) -> Any -> r
elimAny f (Any a) = f a

showAny :: Any -> String
showAny = elimAny show

castAny :: (Typeable a) => Any -> Maybe a
castAny = elimAny cast

justHello :: a -> String
justHello _ = "Hello"

justIntHello :: Int -> String
justIntHello _ = "Hello"
