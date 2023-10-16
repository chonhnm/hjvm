module AccessFlags where

import ClassFile (AccessFlags (..))
import ClassFileConsts
import Data.Bits ((.&.))

class IAccessFlags a where
  is_public :: a -> Bool
  is_final :: a -> Bool
  is_super :: a -> Bool
  is_interface :: a -> Bool
  is_abstract :: a -> Bool
  is_synthetic :: a -> Bool
  is_annotation :: a -> Bool
  is_enum :: a -> Bool
  is_module :: a -> Bool

instance IAccessFlags AccessFlags where
  is_public (AccessFlags flg) = flg .&. jvm_acc_public /= 0
  is_final (AccessFlags flg) = flg .&. jvm_acc_final /= 0 
  is_super (AccessFlags flg) = flg .&. jvm_acc_super /= 0
  is_interface (AccessFlags flg) = flg .&. jvm_acc_interface /= 0
  is_abstract (AccessFlags flg) = flg .&. jvm_acc_abstract /= 0
  is_synthetic (AccessFlags flg) = flg .&. jvm_acc_synthetic /= 0
  is_annotation (AccessFlags flg) = flg .&. jvm_acc_annotation /= 0
  is_enum (AccessFlags flg) = flg .&. jvm_acc_enum /= 0
  is_module (AccessFlags flg) = flg .&. jvm_acc_module /= 0
