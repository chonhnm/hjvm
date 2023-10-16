{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module ClassFileParser where

import ClassFile
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local)
import Data.Foldable (forM_)
import Data.Functor (void)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Parsec
import Text.Printf (printf)
import Util

class ClassFileParser cf where
  getMajorVersion :: cf -> U2
  getConstantPoolInfo :: cf -> ConstantPoolInfo

instance ClassFileParser ClassFile where
  getMajorVersion = majorVersion
  getConstantPoolInfo = constantPool

class CPInfoChecker a where
  checkCPInfo :: a -> CPReader ()

instance CPInfoChecker CPInfo where
  checkCPInfo = checkCPInfo_

checkCPInfo_ :: CPInfo -> CPReader ()
checkCPInfo_ (Constant_Utf8 _) = return ()
checkCPInfo_ (Constant_Integer _) = return ()
checkCPInfo_ (Constant_Float _) = return ()
checkCPInfo_ (Constant_Long _) = do
  idx <- asks envCurIdx
  checkConstantInvalid (idx + 1)
checkCPInfo_ (Constant_Double _) = do
  idx <- asks envCurIdx
  checkConstantInvalid (idx + 1)
checkCPInfo_ (Constant_Class idx) = void $ checkConstantClass idx
checkCPInfo_ (Constant_String idx) = void $ checkConstantUtf8 idx
checkCPInfo_ (Constant_Fieldref fr) = do
  let ConstFieldref cIdx ntIdx = fr
  _ <- checkConstantClass cIdx
  ConstNameAndType nIdx desIdx <- checkConstantNameAndType ntIdx
  _ <- getAndCheckFieldName nIdx
  void $ getAndCheckFieldDesc desIdx
checkCPInfo_ (Constant_Methodref mr) = do
  let ConstMethodref cIdx ntIdx = mr
  _ <- checkConstantClass cIdx
  ConstNameAndType nIdx desIdx <- checkConstantNameAndType ntIdx
  name <- getAndCheckMethodName nIdx
  desc <- getAndCheckMethodDesc desIdx
  when (T.head name == jvm_signature_special)
    $ when
      (name /= T.pack "<init>" || T.last desc /= jvm_signature_void)
    $ lift
    $ Left
    $ ClassFormatError
    $ printf "Unexpected method name and type: %s:%s" name desc
checkCPInfo_ (Constant_InterfaceMethodref imr) = do
  let ConstInterfaceMethodref cIdx ntIdx = imr
  _ <- checkConstantClass cIdx
  ConstNameAndType nIdx desIdx <- checkConstantNameAndType ntIdx
  name <- getAndCheckMethodName nIdx
  _ <- getAndCheckMethodDesc desIdx
  when (T.head name == jvm_signature_special) $
    lift $
      Left $
        ClassFormatError $
          printf "Unexpected interface method name and type: %s:%s" name
-- Already checked by Fieldref or Methodref
checkCPInfo_ (Constant_NameAndType _) = return ()
checkCPInfo_ (Constant_MethodHandle h) = do
  cp <- asks envPool
  let major = cpMajorVersion cp
  let ConstMethodHandle rkind ridx = h
  case rkind of
    x
      | x == REF_getField
          || x == REF_getStatic
          || x == REF_putField
          || x == REF_putStatic ->
          lift $ cpCheckTag JVM_Constant_Fieldref cp ridx
    x
      | x == REF_invokeVirtual
          || x == REF_newInvokeSpecial ->
          lift $ cpCheckTag JVM_Constant_Methodref cp ridx
    x
      | x == REF_invokeStatic
          || x == REF_invokeSpecial ->
          if major < java_8_version
            then lift $ cpCheckTag JVM_Constant_Methodref cp ridx
            else
              lift $
                cpCheckTag JVM_Constant_Methodref cp ridx
                  <> cpCheckTag JVM_Constant_InterfaceMethodref cp ridx
    REF_invokeInterface -> lift $ cpCheckTag JVM_Constant_InterfaceMethodref cp ridx
    _ -> lift $ cfErr $ printf "Unknown reference kind: %s." (show rkind)

  case rkind of
    x
      | x == REF_invokeVirtual
          || x == REF_invokeStatic
          || x == REF_invokeSpecial
          || x == REF_invokeInterface -> do
          tag <- lift $ cpTag cp ridx
          ConstantUtf8 name <- case tag of
            JVM_Constant_Methodref -> checkConstantMethodref_name ridx
            _ -> checkConstantInterfaceMethodref_name ridx
          when (name == T.pack "<init>" || name == T.pack "<clinit>") $
            lift $
              cfErr $
                printf "reference kind \"%s\" do not support method \"%s\"." (show rkind) name
    REF_newInvokeSpecial -> do
      ConstantUtf8 name <- checkConstantMethodref_name ridx
      when (name /= T.pack "<init>") $
        lift $
          cfErr $
            printf "reference kind \"%s\" do not support method \"%s\"." (show rkind) name
    _ -> return ()
checkCPInfo_ (Constant_MethodType idx) = undefined
checkCPInfo_ (Constant_Dynamic attrIdx idx) = undefined
checkCPInfo_ (Constant_InvokeDynamic attrIdx idx) = undefined
checkCPInfo_ (Constant_Module idx) = undefined
checkCPInfo_ (Constant_Package idx) = undefined
checkCPInfo_ Constant_Invalid = undefined

type CPReader = ReaderT Env MyErr

cfErr :: String -> Either AppErr b
cfErr str = Left $ ClassFormatError str

data Env = Env {envPool :: ConstantPoolInfo, envCurIdx :: U2}

checkConstantPoolInfo :: ConstantPoolInfo -> MyErr ()
checkConstantPoolInfo cp =
  do
    let tags = cpTags cp
    let infos = cpInfos cp
    let cnt = cpCount cp
    when (length tags /= fromIntegral cnt || length tags /= length infos) $
      cfErr "Unexpected fatal error."
    when (cnt <= 0) $ cfErr "Constant pool count shoud greater than zero."
    when (head tags /= JVM_Constant_Invalid) $
      cfErr "Constant pool entry at zero is not invalid."
    runReaderT (doCheckCPInfo infos) (Env cp 0)

doCheckCPInfo :: [CPInfo] -> CPReader ()
doCheckCPInfo [] = return ()
doCheckCPInfo (x : xs) = do
  checkCPInfo x
  local incIdx $ doCheckCPInfo xs

incIdx :: Env -> Env
incIdx (Env cp idx) = Env cp (idx + 1)

checkConstantInvalid :: U2 -> CPReader ()
checkConstantInvalid idx = do
  cp <- asks envPool
  lift $ cpCheckTag JVM_Constant_Invalid cp idx

checkConstantUtf8 :: U2 -> CPReader ConstantUtf8
checkConstantUtf8 idx = do
  cp <- asks envPool
  lift $ cpUtf8 cp idx

checkConstantInteger :: U2 -> CPReader ()
checkConstantInteger = undefined

checkConstantFloat :: U2 -> CPReader ()
checkConstantFloat = undefined

checkConstantLong :: U2 -> CPReader ()
checkConstantLong = undefined

checkConstantDouble :: U2 -> CPReader ()
checkConstantDouble = undefined

checkConstantClass :: U2 -> CPReader Text
checkConstantClass = checkUtf8 verifyLegalClassName

checkConstantString :: U2 -> CPReader ()
checkConstantString = undefined

checkConstantFieldref :: U2 -> CPReader ()
checkConstantFieldref = undefined -- no need

checkConstantMethodref :: U2 -> CPReader ConstMethodref
checkConstantMethodref idx = do
  cp <- asks envPool
  lift $ cpMethodref cp idx

checkConstantMethodref_name :: U2 -> CPReader ConstantUtf8
checkConstantMethodref_name idx = do
  ConstMethodref _ nt <- checkConstantMethodref idx
  ConstNameAndType n _ <- checkConstantNameAndType nt
  checkConstantUtf8 n

checkConstantInterfaceMethodref :: U2 -> CPReader ConstInterfaceMethodref
checkConstantInterfaceMethodref idx = do
  cp <- asks envPool
  lift $ cpInterfaceMethodref cp idx

checkConstantInterfaceMethodref_name :: U2 -> CPReader ConstantUtf8
checkConstantInterfaceMethodref_name idx = do
  ConstInterfaceMethodref _ nt <- checkConstantInterfaceMethodref idx
  ConstNameAndType n _ <- checkConstantNameAndType nt
  checkConstantUtf8 n

checkConstantNameAndType :: U2 -> CPReader ConstNameAndType
checkConstantNameAndType idx = do
  cp <- asks envPool
  lift $ cpNameAndType cp idx

checkConstantMethodHandle :: U2 -> CPReader ()
checkConstantMethodHandle = undefined

checkConstantMethodType :: U2 -> CPReader ()
checkConstantMethodType = undefined

checkConstantDynamic :: U2 -> CPReader ()
checkConstantDynamic = undefined

checkConstantInvokeDynamic :: U2 -> CPReader ()
checkConstantInvokeDynamic = undefined

checkConstantModule :: U2 -> CPReader ()
checkConstantModule = undefined

checkConstantPackage :: U2 -> CPReader ()
checkConstantPackage = undefined

--------------------------
checkUtf8 :: (Text -> Maybe String) -> U2 -> CPReader Text
checkUtf8 checker idx = do
  cp <- asks envPool
  ConstantUtf8 name <- lift $ cpUtf8 cp idx
  case checker name of
    Nothing -> return name
    Just str -> lift $ Left $ ClassFormatError str

getAndCheckFieldDesc :: U2 -> CPReader Text
getAndCheckFieldDesc = checkUtf8 runParseFieldDescriptor

getAndCheckMethodDesc :: U2 -> CPReader Text
getAndCheckMethodDesc = checkUtf8 runParseMethodDescriptor

getAndCheckFieldName :: U2 -> CPReader Text
getAndCheckFieldName = checkUtf8 verifyLegalFieldName

getAndCheckMethodName :: U2 -> CPReader Text
getAndCheckMethodName = checkUtf8 verifyLegalMethodName

data LegalTag = LegalClass | LegalField | LegalMethod deriving (Eq)

verifyLegalClassName :: Text -> Maybe String
verifyLegalClassName name
  | T.head name == jvm_signature_array = runParseFieldDescriptor name
  | verifyUnqualifiedName LegalClass name = Nothing
  | otherwise = Just $ printf "Illegal class name \"%s\"." name

verifyLegalFieldName :: Text -> Maybe String
verifyLegalFieldName name
  | verifyUnqualifiedName LegalField name = Nothing
  | otherwise = Just $ printf "Illegal field name \"%s\"." name

verifyLegalMethodName :: Text -> Maybe String
verifyLegalMethodName name
  | name == T.pack "<init>" || name == T.pack "<clinit>" = Nothing
  | verifyUnqualifiedName LegalMethod name = Nothing
  | otherwise = Just $ printf "Illegal method name \"%s\"." name

verifyUnqualifiedName :: LegalTag -> Text -> Bool
verifyUnqualifiedName _ name | T.null name = False
verifyUnqualifiedName _ name
  | T.head name == jvm_signature_slash
      || T.last name == jvm_signature_slash =
      False
verifyUnqualifiedName tag name =
  all
    checkIt
    (T.zip name $ T.snoc (T.tail name) (T.head name))
  where
    checkIt (ch, _)
      | ch == jvm_signature_dot
          || ch == jvm_signature_endclass
          || ch == jvm_signature_array =
          False
    checkIt (ch, ch2)
      | ch == jvm_signature_slash =
          (tag == LegalClass) && (ch2 /= jvm_signature_slash)
    checkIt (ch, _)
      | (ch == jvm_signature_special || ch == jvm_signature_endspecial)
          && tag == LegalMethod =
          False
    checkIt _ = True

type FieldParser = ParsecT Text () Identity

data FieldType = BaseType | ObjectType | ArrayType | VoidType

runParseFieldDescriptor :: Text -> Maybe String
runParseFieldDescriptor name =
  case runP verifyFieldDescriptor () "fielddesc" name of
    Left err -> Just $ show err
    Right _ -> Nothing

runParseMethodDescriptor :: Text -> Maybe String
runParseMethodDescriptor name =
  case runP verifyMethodDescriptor () "methoddesc" name of
    Left err -> Just $ show err
    Right _ -> Nothing

verifyMethodDescriptor :: FieldParser ()
verifyMethodDescriptor = do
  _ <- char jvm_signature_func
  xs <- many verifyFieldType
  when (length xs > 255) $ fail "Method with over 255 parameters"
  _ <- char jvm_signature_endfunc
  verifyReturnDescriptor

verifyFieldDescriptor :: FieldParser ()
verifyFieldDescriptor = verifyFieldType >> eof

verifyReturnDescriptor :: FieldParser ()
verifyReturnDescriptor = do
  verifyFieldType <|> verifyVoidDescriptor

verifyVoidDescriptor :: FieldParser ()
verifyVoidDescriptor = void (char jvm_signature_void)

verifyFieldType :: FieldParser ()
verifyFieldType = verifyBaseType <|> verifyObjectType <|> verifyArrayType

verifyBaseType :: FieldParser ()
verifyBaseType = do
  _ <-
    oneOf
      [ jvm_signature_byte,
        jvm_signature_char,
        jvm_signature_double,
        jvm_signature_float,
        jvm_signature_int,
        jvm_signature_long,
        jvm_signature_short,
        jvm_signature_boolean
      ]
  return ()

verifyObjectType :: FieldParser ()
verifyObjectType = do
  _ <- char jvm_signature_class
  name <- manyTill anyChar (char jvm_signature_endclass)
  forM_ (verifyLegalClassName $ T.pack name) fail

verifyArrayType :: FieldParser ()
verifyArrayType = do
  xs <- many1 $ char jvm_signature_array
  when (length xs > 255) $ fail $ "Array type with over 255 dimensions: " ++ show xs
  verifyFieldType
  return ()