{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module ClassFileChecker (checkAttrLength) where

import ClassFile
import Data.Typeable (typeOf)
import Text.Printf (printf)

class AttrInfoLen a where
  attrLen :: a -> U4

class (AttrInfoLen a) => AttrInfoLenChecker a where
  checkLength :: a -> Maybe String

instance AttrInfoLen AttributeInfo where
  attrLen (AttributeInfo _ attr) = 6 + attrLen attr

instance AttrInfoLen AttrInfo where
  attrLen (ConstantValue _) = 2
  attrLen (Code (CodeAttr _ _ codes ets attrs)) =
    12 + fromIntegral (length codes) + fromIntegral (length ets) * 8 + sum (map attrLen attrs)
  attrLen (StackMapTable _) = 0
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

checkAttrLength :: ClassFile -> Maybe String
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
        Nothing -> Nothing
        Just (Just str, attr) -> Just (str ++ ";attr:" ++ show attr)
        Just _ -> error $ "Unexpected: " ++ show err
  where
    safeHead [] = Nothing
    safeHead (x : _) = Just x