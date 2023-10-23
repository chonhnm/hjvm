{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module ClassFileConsts where

import Data.Word (Word16, Word32, Word64, Word8)

type U1 = Word8

type U2 = Word16

type U4 = Word32

type U8 = Word64

type AttrIndex = U2

type CPIndex = U2

java_classfile_magic :: U4
java_classfile_magic = 0xCAFEBABE

java_min_support_version :: U2
java_min_support_version = 45

-- java version
java_1_5_version :: U2
java_1_5_version = 49

java_6_version :: U2
java_6_version = 50

java_7_version :: U2
java_7_version = 51

java_8_version :: U2
java_8_version = 52

java_9_version :: U2
java_9_version = 53

java_10_version :: U2
java_10_version = 54

java_11_version :: U2
java_11_version = 55

java_12_version :: U2
java_12_version = 56

java_13_version :: U2
java_13_version = 57

java_14_version :: U2
java_14_version = 58

java_15_version :: U2
java_15_version = 59

java_16_version :: U2
java_16_version = 60

java_17_version :: U2
java_17_version = 61

java_18_version :: U2
java_18_version = 62

java_19_version :: U2
java_19_version = 63

java_20_version :: U2
java_20_version = 64

java_21_version :: U2
java_21_version = 65

jvm_classfile_major_version :: U2
jvm_classfile_major_version = 65

-- Type signatures
jvm_signature_slash = '/'

jvm_signature_dot = '.'

jvm_signature_special = '<'

jvm_signature_endspecial = '>'

jvm_signature_array = '['

jvm_signature_byte = 'B'

jvm_signature_char = 'C'

jvm_signature_class = 'L'

jvm_signature_endclass = ';'

jvm_signature_enum = 'E'

jvm_signature_float = 'F'

jvm_signature_double = 'D'

jvm_signature_func = '('

jvm_signature_endfunc = ')'

jvm_signature_int = 'I'

jvm_signature_long = 'J'

jvm_signature_short = 'S'

jvm_signature_void = 'V'

jvm_signature_boolean = 'Z'

-- access flags
jvm_acc_public :: U2
jvm_acc_public = 0x0001

jvm_acc_final :: U2
jvm_acc_final = 0x0010

jvm_acc_super :: U2
jvm_acc_super = 0x0020

jvm_acc_interface :: U2
jvm_acc_interface = 0x0200

jvm_acc_abstract :: U2
jvm_acc_abstract = 0x0400

jvm_acc_synthetic :: U2
jvm_acc_synthetic = 0x1000

jvm_acc_annotation :: U2
jvm_acc_annotation = 0x2000

jvm_acc_enum :: U2
jvm_acc_enum = 0x4000

jvm_acc_module :: U2
jvm_acc_module = 0x8000