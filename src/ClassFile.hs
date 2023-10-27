{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ClassFile where

import Attribute
import ConstantPool
import Data.Typeable (Typeable)
import Util

type Version = (U2, U2, String)

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

newtype AccessFlags = AccessFlags U2 deriving (Show)

data ClassFile = ClassFile
  { minorVersion :: U2,
    majorVersion :: U2,
    constantPool :: ConstantPoolInfo,
    accessFlags :: AccessFlags,
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
      constantPool = emptyConstantPoolInfo,
      accessFlags = AccessFlags 0,
      thisClass = 0,
      superClass = 0,
      interfaces = [],
      fields = [],
      methods = [],
      attributes = []
    }

javaVersion :: U2 -> Maybe String
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
