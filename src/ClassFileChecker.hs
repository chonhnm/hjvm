{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module ClassFileChecker (checkAttrLength) where

import ClassFile
import Data.Typeable (typeOf)
import Text.Printf (printf)
import Util

class AttrInfoLen a where
  attrLen :: a -> U4

class (AttrInfoLen a) => AttrInfoLenChecker a where
  checkLength :: a -> Maybe String

instance AttrInfoLen AttributeInfo where
  attrLen (AttributeInfo _ attr) = 6 + attrLen attr

instance AttrInfoLen AttrInfo where
  attrLen (ConstantValue _) = 2
  attrLen (Code (CodeAttr _ _ codes ets attrs)) =
    12 + fromIntegral (length codes) + 8 * fromIntegral (length ets) + sum (map attrLen attrs)
  attrLen (StackMapTable entries) = 2 + sum (map attrLen entries)
  attrLen (Exceptions xs) = 2 + 2 * fromIntegral (length xs)
  attrLen (InnerClasses xs) = 2 + 8 * fromIntegral (length xs)
  attrLen (EnclosingMethod _ _) = 4
  attrLen Synthetic = 0
  attrLen (Signature _) = 2
  attrLen (SourceFile _) = 2
  attrLen (SourceDebugExtension xs) = fromIntegral $ length xs
  attrLen (LineNumberTable xs) = 2 + 4 * fromIntegral (length xs)
  attrLen (LocalVariableTable xs) = 2 + 10 * fromIntegral (length xs)
  attrLen (LocalVariableTypeTable xs) = 2 + 10 * fromIntegral (length xs)
  attrLen Deprecated = 0
  attrLen (RuntimeVisibleAnnotations _) = 0
  attrLen (RuntimeInvisibleAnnotations _) = 0
  attrLen (RuntimeVisibleParameterAnnotations _) = 0
  attrLen (RuntimeInvisibleParameterAnnotations _) = 0
  attrLen (RuntimeVisibleTypeAnnotations _) = 0
  attrLen (RuntimeInvisibleTypeAnnotations _) = 0
  attrLen (AnnotationDefault _) = 0
  attrLen (BootstrapMethods xs) = 2 + 6 * fromIntegral (length xs)
  attrLen (MethodParameters xs) = 1 + 4 * fromIntegral (length xs)
  attrLen (Module (ModuleAttr _ _ _ reqs exps ops useidxes pvd)) =
    16
      + 6 * fromIntegral (length reqs)
      + 8 * fromIntegral (length exps)
      + 8 * fromIntegral (length ops)
      + 2 * fromIntegral (length useidxes)
      + 6 * fromIntegral (length pvd)
  attrLen (ModulePackages xs) = 2 + 2 * fromIntegral (length xs)
  attrLen (ModuleMainClass _) = 2
  attrLen (NestHost _) = 2
  attrLen (NestMembers xs) = 2 + 2 * fromIntegral (length xs)
  attrLen (Record xs) =
    sum
      ( map
          ( \(RecordComponentInfo _ _ attrs) ->
              6 + sum (map attrLen attrs)
          )
          xs
      )
  attrLen (PermittedSubclasses xs) = 2 + 2 * fromIntegral (length xs)

instance AttrInfoLen StackMapFrame where 
  attrLen (Same_Frame _) = 1
  attrLen (Same_locals_1_stack_item_frame _ xs) = 1 + stacksLen xs   
  attrLen (Same_locals_1_stack_item_frame_extented _ _ xs) = 3 + stacksLen xs
  attrLen Chop_frame{} = 3
  attrLen Same_frame_extented{} = 3
  attrLen (Append_frame _ _ xs) = 3 + stacksLen xs 
  attrLen (Full_frame _ _ locals stacks ) = 7 + stacksLen locals + stacksLen stacks 

stacksLen :: [VerificationTypeInfo] -> U4
stacksLen xs = sum (map attrLen xs)



instance AttrInfoLen VerificationTypeInfo where 
  attrLen Top_variable_info = 1
  attrLen Integer_variable_info = 1
  attrLen Float_variable_info = 1
  attrLen Double_variable_info = 1
  attrLen Long_variable_info = 1
  attrLen Null_variable_info = 1
  attrLen UninitializedThis_variable_info = 1
  attrLen Object_variable_info{} = 3
  attrLen Uninitialized_variable_info{} = 3


instance AttrInfoLenChecker AttributeInfo where
  checkLength (AttributeInfo _ info)
    | typeOf info
        `elem` [ typeOf StackMapTable,
                 typeOf RuntimeVisibleAnnotations,
                 typeOf RuntimeInvisibleAnnotations,
                 typeOf RuntimeVisibleParameterAnnotations,
                 typeOf RuntimeInvisibleParameterAnnotations,
                 typeOf RuntimeVisibleTypeAnnotations,
                 typeOf RuntimeInvisibleTypeAnnotations,
                 typeOf AnnotationDefault
               ] =
        Nothing
  checkLength (AttributeInfo len info) =
    let al = attrLen info
     in if len == al
          then Nothing
          else Just $ printf "Expected: %d, Actual: %d." len al

checkAttrLength :: ClassFile -> MyErr ()
checkAttrLength cf =
  let fxss = map fi_atrributes $ fields cf
      mxss = map mi_attributes $ methods cf
      xs = attributes cf
      err =
        safeHead
          $ filter
            ( \msg -> case msg of
                (Nothing, _) -> False
                (Just _, _) -> True
            )
            . map (\attr -> (checkLength attr, attr))
          $ concat fxss ++ concat mxss ++ xs
   in case err of
        Nothing -> return ()
        Just (Just str, attr) -> classFormatErr (str ++ "Attrinfo:" ++ show attr)
        Just _ -> classFormatErr $ "Unexpected: " ++ show err
  where
    safeHead [] = Nothing
    safeHead (x : _) = Just x