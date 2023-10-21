{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module AccessFlags where

import ClassFile (AccessFlags (..))
import ClassFileConsts
import Data.Bits ((.&.))
import Text.Printf (printf)

data AccessFlag
  = ACC_PUBLIC
  | ACC_FINAL
  | ACC_SUPER
  | ACC_INTERFACE
  | ACC_ABSTRACT
  | ACC_SYNTHETIC
  | ACC_ANNOTATION
  | ACC_ENUM
  | ACC_MODULE
  deriving (Show)

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
  encodeHex :: a -> String
  encodeFlags :: a -> [AccessFlag]

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
  encodeHex (AccessFlags flg) = printf "%#06hx" flg 
  encodeFlags flg =
    foldr
      ( \matcher accu -> case matcher flg of
          Nothing -> accu
          Just x -> x : accu
      )
      []
      matches

matches :: (IAccessFlags a) => [a -> Maybe AccessFlag]
matches =
  [ matchPublic,
    matchFinal,
    matchSuper,
    matchInterface,
    matchAbstract,
    matchSynthetic,
    matchAnnotation,
    matchEnum,
    matchModule
  ]

matchPublic,
  matchFinal,
  matchSuper,
  matchInterface,
  matchAbstract,
  matchSynthetic,
  matchAnnotation,
  matchEnum,
  matchModule ::
    (IAccessFlags a) => a -> Maybe AccessFlag
matchPublic flg = if is_public flg then Just ACC_PUBLIC else Nothing
matchFinal flg = if is_final flg then Just ACC_FINAL else Nothing
matchSuper flg = if is_super flg then Just ACC_SUPER else Nothing
matchInterface flg = if is_interface flg then Just ACC_INTERFACE else Nothing
matchAbstract flg = if is_abstract flg then Just ACC_ABSTRACT else Nothing
matchSynthetic flg = if is_synthetic flg then Just ACC_SYNTHETIC else Nothing
matchAnnotation flg = if is_annotation flg then Just ACC_ANNOTATION else Nothing
matchEnum flg = if is_enum flg then Just ACC_ENUM else Nothing
matchModule flg = if is_module flg then Just ACC_MODULE else Nothing