{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module ConstantPool where

import Control.Monad (when)
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Typeable (Typeable, cast)
import Text.Printf (printf)
import Util

data ConstantPoolInfo = ConstantPoolInfo
  { cpMajorVersion :: U2,
    cpCount :: U2,
    cpTags :: [CPTag],
    cpEntries :: [CPEntry]
  }
  deriving (Show)

emptyConstantPoolInfo :: ConstantPoolInfo
emptyConstantPoolInfo = ConstantPoolInfo 0 0 [] []

data CPEntry
  = Constant_Invalid ConstInvalid
  | Constant_Utf8 ConstUtf8
  | Constant_Integer ConstInteger
  | Constant_Float ConstFloat
  | Constant_Long ConstLong
  | Constant_Double ConstDouble
  | Constant_Class ConstClass
  | Constant_String ConstString
  | Constant_Fieldref ConstFieldref
  | Constant_Methodref ConstMethodref
  | Constant_InterfaceMethodref ConstInterfaceMethodref
  | Constant_NameAndType ConstNameAndType
  | Constant_MethodHandle ConstMethodHandle
  | Constant_MethodType ConstMethodType
  | Constant_Dynamic ConstDynamic
  | Constant_InvokeDynamic ConstInvokeDynamic
  | Constant_Module ConstModule
  | Constant_Package ConstPackage
  deriving (Typeable, Show)

contCPEntry :: (forall a. (Typeable a) => a -> r) -> CPEntry -> r
contCPEntry f (Constant_Invalid x) = f x
contCPEntry f (Constant_Utf8 x) = f x
contCPEntry f (Constant_Integer x) = f x
contCPEntry f (Constant_Float x) = f x
contCPEntry f (Constant_Long x) = f x
contCPEntry f (Constant_Double x) = f x
contCPEntry f (Constant_Class x) = f x
contCPEntry f (Constant_String x) = f x
contCPEntry f (Constant_Fieldref x) = f x
contCPEntry f (Constant_Methodref x) = f x
contCPEntry f (Constant_InterfaceMethodref x) = f x
contCPEntry f (Constant_NameAndType x) = f x
contCPEntry f (Constant_MethodHandle x) = f x
contCPEntry f (Constant_MethodType x) = f x
contCPEntry f (Constant_Dynamic x) = f x
contCPEntry f (Constant_InvokeDynamic x) = f x
contCPEntry f (Constant_Module x) = f x
contCPEntry f (Constant_Package x) = f x

castCPEntry :: forall a. (Typeable a) => CPEntry -> Maybe a
castCPEntry = contCPEntry cast

data ConstInvalid = ConstInvalid deriving (Show)

newtype ConstUtf8 = ConstUtf8 T.Text deriving (Show)

newtype ConstInteger = ConstInteger Int32 deriving (Show)

newtype ConstFloat = ConstFloat Float deriving (Show)

newtype ConstLong = ConstLong Int64 deriving (Show)

newtype ConstDouble = ConstDouble Double deriving (Show)

newtype ConstClass = ConstClass CPIndex deriving (Show)

newtype ConstString = ConstString CPIndex deriving (Show)

data ConstFieldref
  = ConstFieldref
      CPIndex -- class_index
      CPIndex -- name_and_type_index
  deriving (Typeable, Show)

data ConstMethodref
  = ConstMethodref
      CPIndex -- class_index
      CPIndex -- name_and_type_index
  deriving (Typeable, Show)

data ConstInterfaceMethodref
  = ConstInterfaceMethodref
      CPIndex -- class_index
      CPIndex -- name_and_type_index
  deriving (Typeable, Show)

data ConstNameAndType
  = ConstNameAndType
      CPIndex -- name_index
      CPIndex -- descriptor_index
  deriving (Typeable, Show)

data ConstMethodHandle = ConstMethodHandle
  { _reference_kind :: ReferenceKind,
    _reference_index :: CPIndex
  }
  deriving (Typeable, Show)

newtype ConstMethodType
  = ConstMethodType
      CPIndex -- descriptor_index
  deriving (Typeable, Show)

data ConstDynamic
  = ConstDynamic
      AttrIndex -- bootstrap_method_attr_index
      CPIndex -- name_and_type_index
  deriving (Typeable, Show)

data ConstInvokeDynamic
  = ConstInvokeDynamic
      AttrIndex -- bootstrap_method_attr_index
      CPIndex -- name_and_type_index
  deriving (Typeable, Show)

newtype ConstModule
  = ConstModule
      CPIndex -- name_index
  deriving (Typeable, Show)

newtype ConstPackage
  = ConstPackage
      CPIndex -- name_index
  deriving (Typeable, Show)

data CPTag
  = JVM_Constant_Invalid
  | JVM_Constant_Utf8
  | JVM_Constant_Integer
  | JVM_Constant_Float
  | JVM_Constant_Long
  | JVM_Constant_Double
  | JVM_Constant_Class
  | JVM_Constant_String
  | JVM_Constant_Fieldref
  | JVM_Constant_Methodref
  | JVM_Constant_InterfaceMethodref
  | JVM_Constant_NameAndType
  | JVM_Constant_MethodHandle
  | JVM_Constant_MethodType
  | JVM_Constant_Dynamic
  | JVM_Constant_InvokeDynamic
  | JVM_Constant_Module
  | JVM_Constant_Package
  deriving (Show, Eq)

data ReferenceKind
  = REF_none -- 0
  | REF_getField -- 1
  | REF_getStatic -- 2
  | REF_putField -- 3
  | REF_putStatic -- 4
  | REF_invokeVirtual -- 5
  | REF_invokeStatic -- 6
  | REF_invokeSpecial -- 7
  | REF_newInvokeSpecial -- 8
  | REF_invokeInterface -- 9
  deriving (Enum, Eq, Show)

class IConstantPool cp where
  cpCheckIndex :: cp -> U2 -> MyErr ()
  cpCheckTag :: CPTag -> cp -> U2 -> MyErr ()
  cpElement :: cp -> U2 -> MyErr CPEntry
  cpTag :: cp -> U2 -> MyErr CPTag
  cpEntry :: (Typeable a) => cp -> U2 -> MyErr a

instance IConstantPool ConstantPoolInfo where
  cpCheckIndex cp n = do
    let count = cpCount cp
    if n >= 1 && n < count
      then Right ()
      else Left $ PE $ PoolOutOfBoundsException $ printf "Index: %d, constant pool count: %d." n count
  cpElement cp n = do
    cpCheckIndex cp n
    return $ cpEntries cp !! fromIntegral n
  cpTag cp n = do
    cpCheckIndex cp n
    return $ cpTags cp !! fromIntegral n
  cpCheckTag tag cp n = do
    cpCheckIndex cp n
    let atag = cpTags cp !! fromIntegral n
    when (tag /= atag) $
      Left $
        ClassFormatError $
          printf "Expected: %s, Actual: %s" (show tag) (show atag)
  cpEntry cp idx = do
    info <- cpElement cp idx
    case castCPEntry info of
      Nothing -> unmatchedErr info idx
      Just x -> return x

unmatchedErr :: CPEntry -> U2 -> MyErr a
unmatchedErr actual idx =
  Left $
    PE $
      PoolUnmatchedType $
        printf
          "Unmatched Constant Pool Entry. Actual: %s, Index: %d"
          (show actual)
          idx