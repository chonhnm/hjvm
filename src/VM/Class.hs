module VM.Class where

import ClassFile (AccessFlags, FieldInfo, MethodInfo)
import ClassFileConsts (U4)
import ConstantPool (ConstantPoolInfo)
import Data.Text qualified as T

newtype ClassId = ClassId U4

data Class = Class
  { cl_id :: ClassId,
    cl_name :: T.Text,
    cl_constants :: ConstantPoolInfo,
    cl_flags :: AccessFlags,
    cl_superclass :: Maybe Class,
    cl_interfaces :: [Class],
    cl_fields :: [FieldInfo],
    cl_methods :: [MethodInfo]
  }