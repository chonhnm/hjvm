{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TypeFamilies #-}
module ConstantPool where

import ClassFileConsts (AttrIndex, CPIndex, U2)
import Control.Monad (when)
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Typeable (Typeable, cast)
import Text.Printf (printf)
import Util (AppErr (ClassFormatError, PE), CheckedError (..), MyErr)

data ConstantPoolInfo = ConstantPoolInfo
  { cpMajorVersion :: U2,
    cpCount :: U2,
    cpTags :: [CPEntryTag],
    cpInfos :: [CPEntryAny]
  }
  deriving (Show)

emptyConstantPoolInfo :: ConstantPoolInfo
emptyConstantPoolInfo = ConstantPoolInfo 0 0 [] []

class ConstantPool cp where
  cpCheckIndex :: cp -> U2 -> MyErr ()
  cpCheckTag :: CPEntryTag -> cp -> U2 -> MyErr ()
  cpElement :: cp -> U2 -> MyErr CPEntryAny
  cpTag :: cp -> U2 -> MyErr CPEntryTag
  cpEntry :: forall a. (Typeable a) => cp -> U2 -> MyErr a

instance ConstantPool ConstantPoolInfo where
  cpCheckIndex cp n = do
    let count = cpCount cp
    if n >= 1 && n < count
      then Right ()
      else
        Left $
          PE $
            PoolOutOfBoundsException $
              printf "Index: %d, constant pool count: %d." n count
  cpElement cp n = do
    cpCheckIndex cp n
    return $ cpInfos cp !! fromIntegral n
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
  cpEntry cp n = do
    entry <- cpElement cp n
    case castCPEntry entry of
      Nothing ->
        Left $
          PE $
            PoolUnmatchedType $
              printf
                "Unmatched Constant Pool Entry at %d: %s."
                n
                (show entry)
      Just x -> Right x

-- Constant Pool Entry
data CPEntryTag
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

data CPEntryAny = forall a. (ICPEntry a, HasConstraintChecker a) => CPEntryAny (CPEntry a)

instance Show CPEntryAny where
  show = contCPEntry show

contCPEntry :: (forall a. (ICPEntry a, HasConstraintChecker a) => a -> r) -> CPEntryAny -> r
contCPEntry f (CPEntryAny a) = f (unwrapCPEntry a)

castCPEntry :: (Typeable a) => CPEntryAny -> Maybe a
castCPEntry = contCPEntry cast

class HasConstraintChecker a where
  type HasCPReader a 
  checkConstraint :: a -> HasCPReader ()

checkCPEntry :: CPEntryAny -> HasCPReader ()
checkCPEntry = contCPEntry checkConstraint


class (Typeable a, Show a) => ICPEntry a

instance ICPEntry ()

instance ICPEntry ConstUtf8

instance ICPEntry ConstInteger

instance ICPEntry ConstFloat

instance ICPEntry ConstLong

instance ICPEntry ConstDouble

instance ICPEntry ConstClass

instance ICPEntry ConstString

instance ICPEntry ConstFieldref

instance ICPEntry ConstMethodref

instance ICPEntry ConstInterfaceMethodref

instance ICPEntry ConstNameAndType

instance ICPEntry ConstMethodHandle

instance ICPEntry ConstMethodType

instance ICPEntry ConstDynamic

instance ICPEntry ConstInvokeDynamic

instance ICPEntry ConstModule

instance ICPEntry ConstPackage

data CPEntry a where
  Constant_Invalid :: CPEntry ()
  Constant_Utf8 :: ConstUtf8 -> CPEntry ConstUtf8
  Constant_Integer :: ConstInteger -> CPEntry ConstInteger
  Constant_Float :: ConstFloat -> CPEntry ConstFloat
  Constant_Long :: ConstLong -> CPEntry ConstLong
  Constant_Double :: ConstDouble -> CPEntry ConstDouble
  Constant_Class :: ConstClass -> CPEntry ConstClass
  Constant_String :: ConstString -> CPEntry ConstString
  Constant_Fieldref :: ConstFieldref -> CPEntry ConstFieldref
  Constant_Methodref :: ConstMethodref -> CPEntry ConstMethodref
  Constant_InterfaceMethodref :: ConstInterfaceMethodref -> CPEntry ConstInterfaceMethodref
  Constant_NameAndType :: ConstNameAndType -> CPEntry ConstNameAndType
  Constant_MethodHandle :: ConstMethodHandle -> CPEntry ConstMethodHandle
  Constant_MethodType :: ConstMethodType -> CPEntry ConstMethodType
  Constant_Dynamic :: ConstDynamic -> CPEntry ConstDynamic
  Constant_InvokeDynamic :: ConstInvokeDynamic -> CPEntry ConstInvokeDynamic
  Constant_Module :: ConstModule -> CPEntry ConstModule
  Constant_Package :: ConstPackage -> CPEntry ConstPackage
  deriving (Typeable)

unwrapCPEntry :: CPEntry a -> a
unwrapCPEntry Constant_Invalid = ()
unwrapCPEntry (Constant_Utf8 x) = x
unwrapCPEntry (Constant_Integer x) = x
unwrapCPEntry (Constant_Float x) = x
unwrapCPEntry (Constant_Long x) = x
unwrapCPEntry (Constant_Double x) = x
unwrapCPEntry (Constant_Class x) = x
unwrapCPEntry (Constant_String x) = x
unwrapCPEntry (Constant_Fieldref x) = x
unwrapCPEntry (Constant_Methodref x) = x
unwrapCPEntry (Constant_InterfaceMethodref x) = x
unwrapCPEntry (Constant_NameAndType x) = x
unwrapCPEntry (Constant_MethodHandle x) = x
unwrapCPEntry (Constant_MethodType x) = x
unwrapCPEntry (Constant_Dynamic x) = x
unwrapCPEntry (Constant_InvokeDynamic x) = x
unwrapCPEntry (Constant_Module x) = x
unwrapCPEntry (Constant_Package x) = x

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