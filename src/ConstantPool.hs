{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module ConstantPool where

import Control.Monad (when)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Typeable (Typeable)
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
  = Constant_Invalid
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

newtype ConstUtf8 = ConstUtf8 T.Text deriving (Show)

newtype ConstInteger = ConstInteger Int32 deriving (Typeable, Show)

newtype ConstFloat = ConstFloat Float deriving (Typeable, Show)

newtype ConstLong = ConstLong Int64 deriving (Typeable, Show)

newtype ConstDouble = ConstDouble Double deriving (Typeable, Show)

newtype ConstClass = ConstClass CPIndex deriving (Typeable, Show)

newtype ConstString = ConstString CPIndex deriving (Typeable, Show)

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

class ConstantPool cp where
  cpCheckIndex :: cp -> U2 -> MyErr ()
  cpCheckTag :: CPTag -> cp -> U2 -> MyErr ()
  cpElement :: cp -> U2 -> MyErr CPEntry
  cpTag :: cp -> U2 -> MyErr CPTag
  cpEntry :: (Typeable a) => cp -> U2 -> MyErr a

instance ConstantPool ConstantPoolInfo where
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
  cpEntry = cpType

cpType :: (Typeable a) => ConstantPoolInfo -> U2 -> MyErr a
cpType cp idx = do
  dyn <- cpDynamic cp idx
  case fromDynamic dyn of
    Nothing -> unmatchedErr dyn idx
    Just x -> return x

cpDynamic :: ConstantPoolInfo -> U2 -> MyErr Dynamic
cpDynamic cp idx = do
  info <- cpElement cp idx
  case info of
    Constant_Invalid -> Right $ toDyn ()
    Constant_Utf8 x -> Right $ toDyn x
    Constant_Integer x -> Right $ toDyn x
    Constant_Float x -> Right $ toDyn x
    Constant_Long x -> Right $ toDyn x
    Constant_Double x -> Right $ toDyn x
    Constant_Class x -> Right $ toDyn x
    Constant_String x -> Right $ toDyn x
    Constant_Fieldref x -> Right $ toDyn x
    Constant_Methodref x -> Right $ toDyn x
    Constant_InterfaceMethodref x -> Right $ toDyn x
    Constant_NameAndType x -> Right $ toDyn x
    Constant_MethodHandle x -> Right $ toDyn x
    Constant_MethodType x -> Right $ toDyn x
    Constant_Dynamic x -> Right $ toDyn x
    Constant_InvokeDynamic x -> Right $ toDyn x
    Constant_Module x -> Right $ toDyn x
    Constant_Package x -> Right $ toDyn x

unmatchedErr :: Dynamic -> U2 -> MyErr a
unmatchedErr actual idx =
  Left $
    PE $
      PoolUnmatchedType $
        printf
          "Unmatched Constant Pool Entry. Actual: %s, Index: %d"
          (show actual)
          idx