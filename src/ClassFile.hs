{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module ClassFile where

import Data.Int (Int32, Int64)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Data.Tuple (swap)
import Data.Word (Word16, Word32, Word64, Word8)

type U1 = Word8

type U2 = Word16

type U4 = Word32

type U8 = Word64

type Magic = U4

type MinorVersion = U2

type MajorVersion = U2

type Version = (U2, U2, String)

type ConstantPoolCount = U2

data ConstantPoolInfo = ConstantPoolInfo
  { cptag :: U1,
    cpInfo :: CPInfo
  }

type Attr_Index = U2

type CP_Index = U2

data CPInfo
  = Constant_Utf8 T.Text
  | Constant_Integer Int32
  | Constant_Float Float
  | Constant_Long Int64
  | Constant_Double Double
  | Constant_Class {_name_index :: CP_Index}
  | Constant_String {_string_index :: CP_Index}
  | Constant_Fieldref {_class_index :: CP_Index, _name_and_type_index :: CP_Index}
  | Constant_Methodref {_class_index :: CP_Index, _name_and_type_index :: CP_Index}
  | Constant_InterfaceMethodref {_class_index :: CP_Index, _name_and_type_index :: CP_Index}
  | Constant_NameAndType {_name_index :: CP_Index, _descriptor_index :: CP_Index}
  | Constant_MethodHandle {_reference_kind :: ReferenceKind, _reference_index :: CP_Index}
  | Constant_MethodType {_descriptor_index :: CP_Index}
  | Constant_Dynamic {_bootstrap_method_attr_index :: Attr_Index, _name_and_type_index :: CP_Index}
  | Constant_InvokeDynamic {_bootstrap_method_attr_index :: Attr_Index, _name_and_type_index :: CP_Index}
  | Constant_Module {_name_index :: CP_Index}
  | Constant_Package {_name_index :: CP_Index}

data ReferenceKind
  = REF_getField
  | REF_getStatic
  | REF_putField
  | REF_putStatic
  | REF_invokeVirtual
  | REF_invokeStatic
  | REF_invokeSpecial
  | REF_newInvokeSpecial
  | REF_invokeInterface
  deriving (Eq, Ord)

refkindTable :: [(ReferenceKind, Int)]
refkindTable =
  [ (REF_getField, 1),
    (REF_getStatic, 2),
    (REF_putField, 3),
    (REF_putStatic, 4),
    (REF_invokeVirtual, 5),
    (REF_invokeStatic, 6),
    (REF_invokeSpecial, 7),
    (REF_newInvokeSpecial, 8),
    (REF_invokeInterface, 9)
  ]

instance Enum ReferenceKind where
  toEnum = fromJust . flip lookup (map swap refkindTable)
  fromEnum = fromJust . flip lookup refkindTable

data FieldInfo

data MethodInfo

data AttributeInfo

data ClassFile = ClassFile
  { magic :: U4,
    minorVersion :: U2,
    majorVersion :: U2,
    constantPoolCount :: U2,
    constantPool :: [ConstantPoolInfo],
    accessFlags :: U2,
    thisClass :: U2,
    superClass :: U2,
    inteterfacesCount :: U2,
    interfaces :: [U2],
    fieldsCount :: U2,
    fields :: [FieldInfo],
    methodsCount :: U2,
    methods :: [MethodInfo],
    attributesCount :: U2,
    attributes :: [AttributeInfo]
  }

javaVersion :: Word16 -> Maybe String
javaVersion version =
  case version of
    45 -> Just "1.0.2/1.1"
    46 -> Just "1.2"
    47 -> Just "1.3"
    48 -> Just "1.4"
    49 -> Just "5.0"
    50 -> Just "6"
    51 -> Just "7"
    52 -> Just "8"
    53 -> Just "9"
    54 -> Just "10"
    55 -> Just "11"
    56 -> Just "12"
    57 -> Just "13"
    58 -> Just "14"
    59 -> Just "15"
    60 -> Just "16"
    61 -> Just "17"
    62 -> Just "18"
    63 -> Just "19"
    64 -> Just "20"
    65 -> Just "21"
    _ -> Nothing