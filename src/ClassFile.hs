{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ClassFile where

import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Typeable (TypeRep, Typeable, typeOf)
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric (showHex)

type U1 = Word8

type U2 = Word16

type U4 = Word32

type U8 = Word64

type Version = (U2, U2, String)

type ConstantPoolCount = U2

data ConstantPoolInfo = ConstantPoolInfo
  { cptag :: U1,
    cpInfo :: CPInfo
  }

type AttrIndex = U2

type CPIndex = U2

data CPInfo
  = Constant_Utf8 ConstantUtf8
  | Constant_Integer ConstantInteger
  | Constant_Float ConstantFloat
  | Constant_Long ConstantLong
  | Constant_Double ConstantDouble
  | Constant_Class {_name_index :: CPIndex}
  | Constant_String {_string_index :: CPIndex}
  | Constant_Fieldref {_class_index :: CPIndex, _name_and_type_index :: CPIndex}
  | Constant_Methodref {_class_index :: CPIndex, _name_and_type_index :: CPIndex}
  | Constant_InterfaceMethodref {_class_index :: CPIndex, _name_and_type_index :: CPIndex}
  | Constant_NameAndType {_name_index :: CPIndex, _descriptor_index :: CPIndex}
  | Constant_MethodHandle {_reference_kind :: ReferenceKind, _reference_index :: CPIndex}
  | Constant_MethodType {_descriptor_index :: CPIndex}
  | Constant_Dynamic {_bootstrap_method_attr_index :: AttrIndex, _name_and_type_index :: CPIndex}
  | Constant_InvokeDynamic {_bootstrap_method_attr_index :: AttrIndex, _name_and_type_index :: CPIndex}
  | Constant_Module {_name_index :: CPIndex}
  | Constant_Package {_name_index :: CPIndex}
  deriving (Typeable, Show)

newtype ConstantUtf8 = ConstantUtf8 T.Text deriving (Show)

newtype ConstantInteger = ConstantInteger Int32 deriving (Show)

newtype ConstantFloat = ConstantFloat Float deriving (Show)

newtype ConstantLong = ConstantLong Int64 deriving (Show)

newtype ConstantDouble = ConstantDouble Double deriving (Show)

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
  deriving (Enum, Show)

data ClassAccessFlag
  = ACC_PUBLIC
  | ACC_FINAL
  | ACC_SUPER
  | ACC_INTERFACE
  | ACC_ABSTRACT
  | ACC_SYNTHETIC
  | ACC_ANNOTATION
  | ACC_ENUM
  | ACC_MODULE

instance Enum ClassAccessFlag where
  fromEnum a = case a of
    ACC_PUBLIC -> 0x0001
    ACC_FINAL -> 0x0010
    ACC_SUPER -> 0x0020
    ACC_INTERFACE -> 0x0200
    ACC_ABSTRACT -> 0x0400
    ACC_SYNTHETIC -> 0x1000
    ACC_ANNOTATION -> 0x2000
    ACC_ENUM -> 0x4000
    ACC_MODULE -> 0x8000
  toEnum a = case a of
    0x0001 -> ACC_PUBLIC
    0x0010 -> ACC_FINAL
    0x0020 -> ACC_SUPER
    0x0200 -> ACC_INTERFACE
    0x0400 -> ACC_ABSTRACT
    0x1000 -> ACC_SYNTHETIC
    0x2000 -> ACC_ANNOTATION
    0x4000 -> ACC_ENUM
    0x8000 -> ACC_MODULE
    _ -> error $ "Invalid Class Access Flag: " ++ show (showHex a "")

data FieldInfo = FieldInfo
  { fi_accessFlags :: FieldAccessFlag,
    fi_nameIndex :: CPIndex,
    fi_descriptor_index :: CPIndex,
    fi_atrributes :: [AttributeInfo]
  }
  deriving (Typeable, Show)

type FieldAccessFlag = U2

data MethodInfo = MethodInfo
  { mi_accessFlags :: MethodAccessFlag,
    mi_nameIndex :: CPIndex,
    mi_descriptor_index :: CPIndex,
    mi_attributes :: [AttributeInfo]
  }
  deriving (Show)

type MethodAccessFlag = U2

data AttributeInfo = AttributeInfo
  { attr_length :: U4,
    attr_info :: AttrInfo
  }
  deriving (Show)

data AttrInfo
  = ConstantValue {_constantvalue_index :: CPIndex}
  | Code CodeAttr
  | StackMapTable {_entries :: [StackMapFrame]}
  | Exceptions {_exception_index_table :: [CPIndex]}
  | InnerClasses {_classes :: [InnerClass]}
  | EnclosingMethod {_ai_class_index :: CPIndex, _ai_method_index :: CPIndex}
  | Synthetic
  | Signature CPIndex
  | SourceFile CPIndex
  | SourceDebugExtension [U1]
  | LineNumberTable [LineNumber]
  | LocalVariableTable [LocalVariable]
  | LocalVariableTypeTable [LocalVariableType]
  | Deprecated
  | RuntimeVisibleAnnotations [Annotation]
  | RuntimeInvisibleAnnotations [Annotation]
  | RuntimeVisibleParameterAnnotations [[Annotation]]
  | RuntimeInvisibleParameterAnnotations [[Annotation]]
  | RuntimeVisibleTypeAnnotations [TypeAnnotation]
  | RuntimeInvisibleTypeAnnotations [TypeAnnotation]
  | AnnotationDefault ElementValue
  | BootstrapMethods [BootstrapMethod]
  | MethodParameters [(CPIndex, U2)]
  | Module ModuleAttr
  | ModulePackages [U2]
  | ModuleMainClass CPIndex
  | NestHost CPIndex
  | NestMembers [CPIndex]
  | Record [RecordComponentInfo]
  | PermittedSubclasses [CPIndex]
  deriving (Typeable, Show)

data RecordComponentInfo = RecordComponentInfo
  { rc_name_index :: CPIndex,
    rc_descriptor_index :: CPIndex,
    rc_attributes :: [AttributeInfo]
  }
  deriving (Show)

data ModuleAttr = ModuleAttr
  { module_name_index :: U2,
    module_flags :: U2,
    module_version_index :: U2,
    requires :: [(U2, U2, U2)],
    exports :: [(U2, U2, [U2])],
    opens :: [(U2, U2, [U2])],
    uses_index :: [U2],
    provides :: [(U2, [U2])]
  }
  deriving (Show)

data BootstrapMethod = BootstrapMethod
  { bootstrap_method_ref :: U2,
    bootstrap_arguments :: [U2]
  }
  deriving (Show)

data TypeAnnotation = TypeAnnotation
  { ta_target_type :: U1,
    ta_target_info :: TypeInfo,
    ta_target_path :: TypePath,
    ta_type_index :: CPIndex,
    ta_element_value_pairs :: [(CPIndex, ElementValue)]
  }
  deriving (Show)

data TypeInfo
  = Type_parameter_target U1
  | Supertype_target U2
  | Type_parameter_bound_target U1 U1
  | Empty_target
  | Formal_parameter_target U1
  | Throws_target U2
  | Localvar_target [(U2, U2, U2)]
  | Catch_target U2
  | Offset_target U2
  | Type_argument_target U2 U1
  deriving (Show)

type TypePath = [(U1, U1)]

data ElementValue
  = EV_byte CPIndex
  | EV_char CPIndex
  | EV_double CPIndex
  | EV_float CPIndex
  | EV_int CPIndex
  | EV_long CPIndex
  | EV_short CPIndex
  | EV_boolean CPIndex
  | EV_string CPIndex
  | EV_enum CPIndex CPIndex
  | EV_class CPIndex
  | EV_anno Annotation
  | EV_array [ElementValue]
  deriving (Show)

data Annotation = Annotation
  { anno_type_index :: U2,
    anno_element_value_pairs :: [(U2, ElementValue)]
  }
  deriving (Show)

data LocalVariableType = LocalVariableType
  { lvt_start_pc :: U2,
    lvt_length :: U2,
    lvt_name_index :: U2,
    lvt_sigature_index :: U2,
    lvt_index :: U2
  }
  deriving (Show)

data LocalVariable = LocalVariable
  { lv_start_pc :: U2,
    lv_length :: U2,
    lv_name_index :: U2,
    lv_descriptor_index :: U2,
    lv_index :: U2
  }
  deriving (Show)

data LineNumber = LineNumber {_start_pc :: U2, _line_number :: U2} deriving (Show)

data InnerClass = InnerClass
  { _inner_class_info_index :: CPIndex,
    _outer_class_info_index :: CPIndex,
    _inner_name_index :: CPIndex,
    _inner_class_access_flags :: U2
  }
  deriving (Show)

data StackMapFrame
  = Same_Frame {frame_type :: U1} -- 0-63
  | Same_locals_1_stack_item_frame
      { frame_type :: U1, -- 64-127
        _stack :: [VerificationTypeInfo]
      }
  | Same_locals_1_stack_item_frame_extented
      { frame_type :: U1, -- 247
        _offset_delta :: U2,
        _stack :: [VerificationTypeInfo]
      }
  | Chop_frame
      { frame_type :: U1, -- 248-250
        _offset_delta :: U2
      }
  | Same_frame_extented
      { frame_type :: U1, -- 251
        _offset_delta :: U2
      }
  | Append_frame
      { frame_type :: U1, -- 252-254
        _offset_delta :: U2,
        _locals :: [VerificationTypeInfo]
      }
  | Full_frame
      { frame_type :: U1, -- 255
        _offset_delta :: U2,
        _number_of_locals :: U2,
        _locals :: [VerificationTypeInfo],
        _number_of_stack_items :: U2,
        _stack :: [VerificationTypeInfo]
      }
  deriving (Show)

data VerificationTypeInfo
  = Top_variable_info -- 0
  | Integer_variable_info -- 1
  | Float_variable_info -- 2
  | Double_variable_info -- 3
  | Long_variable_info -- 4
  | Null_variable_info -- 5
  | UninitializedThis_variable_info -- 6
  | Object_variable_info {_cpool_index :: CPIndex} -- 7
  | Uninitialized_variable_info {_offset :: U2} -- 8
  deriving (Show)

data CodeAttr = CodeAttr
  { ca_max_stack :: U2,
    ca_max_locals :: U2,
    ca_code_length :: U4,
    ca_code :: [U1],
    ca_exception_table_length :: U2,
    ca_exception_table :: [ExceptionTable],
    ca_attributes_count :: U2,
    ca_attributes :: [AttributeInfo]
  }
  deriving (Show)

data ExceptionTable = ExceptionTable
  { et_start_pc :: U2,
    et_end_pc :: U2,
    et_handler_pc :: U2,
    et_catch_type :: U2
  }
  deriving (Show)

data ClassFile = ClassFile
  { minorVersion :: U2,
    majorVersion :: U2,
    constantPool :: [CPInfo],
    accessFlags :: U2,
    thisClass :: CPIndex,
    superClass :: CPIndex,
    interfaces :: [CPIndex],
    fields :: [FieldInfo],
    methods :: [MethodInfo],
    attributes :: [AttributeInfo]
  }
  deriving (Typeable, Show)

emptyClassFile :: ClassFile
emptyClassFile =
  ClassFile
    { minorVersion = 0,
      majorVersion = 0,
      constantPool = [],
      accessFlags = 0,
      thisClass = 0,
      superClass = 0,
      interfaces = [],
      fields = [],
      methods = [],
      attributes = []
    }

javaVersion :: Word16 -> Maybe String
javaVersion version =
  case version of
    45 -> Just "1.0.2"
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

class Seq s where
  fetch :: s a -> U2 -> Maybe a

class ConstantPool s a where
  convertIndex :: s a -> Int -> Maybe Int
  constPool :: s a -> Int -> a
  constPoolWithTypeCheck :: TypeRep -> s a -> Int -> CPInfo
  constUtf8 :: s a -> Int -> ConstantUtf8
  constInteger :: s a -> Int -> ConstantInteger
  constFloat :: s a -> Int -> ConstantFloat
  constLong :: s a -> Int -> ConstantLong
  constDouble :: s a -> Int -> ConstantDouble

instance ConstantPool [] CPInfo where
  convertIndex :: [CPInfo] -> Int -> Maybe Int
  convertIndex xs n =
    if n > 0 && n <= length xs
      then Just (n - 1)
      else Nothing
  constPool :: [CPInfo] -> Int -> CPInfo
  constPool xs n = case convertIndex xs n of
    Nothing -> error $ "PoolOutOfBoundsException: " ++ show n ++ "; " ++ show xs
    Just idx -> xs !! idx
  constPoolWithTypeCheck :: TypeRep -> [CPInfo] -> Int -> CPInfo
  constPoolWithTypeCheck tr xs n =
    let cp = constPool xs n
     in if typeOf cp == tr
          then cp
          else error "PoolUnmatchedType"
  constUtf8 cp n = let Constant_Utf8 x = constPool cp n in x
  constInteger cp n = let Constant_Integer x = constPool cp n in x
  constFloat cp n = let Constant_Float x = constPool cp n in x
  constLong cp n = let Constant_Long x = constPool cp n in x
  constDouble cp n = let Constant_Double x = constPool cp n in x
