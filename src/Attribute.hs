{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Attribute where

import Data.Typeable (Typeable)
import Util

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
        _locals :: [VerificationTypeInfo],
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
    ca_code :: [U1],
    ca_exception_table :: [ExceptionTable],
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
