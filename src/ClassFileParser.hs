{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module ClassFileParser where

import AccessFlags (IAccessFlags (is_module))
import ClassFile
import ConstantPool
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks, local)
import Data.Functor (void)
import Data.Functor.Identity (Identity)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Parsec
import Text.Parsec.Error (errorMessages, messageString)
import Text.Printf (printf)
import Util

type CPReader = ReaderT Env MyErr

data Env = Env
  { envClassFile :: ClassFile,
    envPool :: ConstantPoolInfo,
    envCurPoolIdx :: U2
  }

class ClassFileParser cf where
  getThisClassName :: cf -> MyErr ConstUtf8
  getSuperClassName :: cf -> MyErr ConstUtf8
  getBootstapMethods :: cf -> [BootstrapMethod]
  getClassName :: cf -> U2 -> MyErr ConstUtf8
  getUtf8Name :: cf -> U2 -> MyErr ConstUtf8

instance ClassFileParser ClassFile where
  getBootstapMethods cf =
    let attrs = attributes cf
        infos = map attr_info attrs
     in foldl bt [] infos
    where
      bt [] (BootstrapMethods xs) = xs
      bt [] _ = []
      bt val _ = val
  getClassName cf idx = do
    let cp = cf.constantPool
    ConstClass nidx <- cpEntry cp idx
    cpEntry cp nidx
  getThisClassName cf = getClassName cf cf.thisClass
  getSuperClassName cf = getClassName cf cf.superClass
  getUtf8Name cf idx = do
    let cp = cf.constantPool
    cpEntry cp idx

class CPInfoChecker a where
  checkCPInfo :: a -> CPReader ()

instance CPInfoChecker CPEntry where
  checkCPInfo = checkCPInfo_

checkCPInfo_ :: CPEntry -> CPReader ()
checkCPInfo_ (Constant_Utf8 _) = return ()
checkCPInfo_ (Constant_Integer _) = return ()
checkCPInfo_ (Constant_Float _) = return ()
checkCPInfo_ (Constant_Long _) = do
  idx <- asks envCurPoolIdx
  checkConstantInvalid (idx + 1)
checkCPInfo_ (Constant_Double _) = do
  idx <- asks envCurPoolIdx
  checkConstantInvalid (idx + 1)
checkCPInfo_ (Constant_Class _) = do
  idx <- asks envCurPoolIdx
  void $ checkConstantClass idx
checkCPInfo_ (Constant_String cs) = do
  let ConstString idx = cs
  void $ checkConstantUtf8 idx
checkCPInfo_ (Constant_Fieldref fr) = do
  let ConstFieldref cIdx ntIdx = fr
  _ <- checkConstantClass cIdx
  ConstNameAndType nIdx desIdx <- checkConstantNameAndType ntIdx
  _ <- checkFieldName nIdx
  void $ checkFieldDesc desIdx
checkCPInfo_ (Constant_Methodref mr) = do
  let ConstMethodref cIdx ntIdx = mr
  _ <- checkConstantClass cIdx
  ConstNameAndType nIdx desIdx <- checkConstantNameAndType ntIdx
  ConstUtf8 name <- checkMethodName nIdx
  ConstUtf8 desc <- checkMethodDesc desIdx
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
  ConstUtf8 name <- checkMethodName nIdx
  _ <- checkMethodDesc desIdx
  when (T.head name == jvm_signature_special) $
    lift $
      Left $
        ClassFormatError $
          printf "Unexpected interface method name and type: %s:%s" name
-- Already checked by Fieldref or Methodref or Dynamic or InvokeDynamic
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
    _ -> lift $ classFormatErr $ printf "Unknown reference kind: %s." (show rkind)

  case rkind of
    x
      | x == REF_invokeVirtual
          || x == REF_invokeStatic
          || x == REF_invokeSpecial
          || x == REF_invokeInterface -> do
          tag <- lift $ cpTag cp ridx
          ConstUtf8 name <- case tag of
            JVM_Constant_Methodref -> checkConstantMethodref_name ridx
            _ -> checkConstantInterfaceMethodref_name ridx
          when (name == T.pack "<init>" || name == T.pack "<clinit>") $
            lift $
              classFormatErr $
                printf "reference kind \"%s\" do not support method \"%s\"." (show rkind) name
    REF_newInvokeSpecial -> do
      ConstUtf8 name <- checkConstantMethodref_name ridx
      when (name /= T.pack "<init>") $
        lift $
          classFormatErr $
            printf "reference kind \"%s\" do not support method \"%s\"." (show rkind) name
    _ -> return ()
checkCPInfo_ (Constant_MethodType (ConstMethodType idx)) = void $ checkMethodDesc idx
checkCPInfo_ (Constant_Dynamic (ConstDynamic attrIdx idx)) = do
  checkBootstrapAttrIdx attrIdx
  ConstNameAndType _ dIdx <- checkConstantNameAndType idx
  void $ checkFieldDesc dIdx
checkCPInfo_ (Constant_InvokeDynamic (ConstInvokeDynamic attrIdx idx)) = do
  checkBootstrapAttrIdx attrIdx
  ConstNameAndType _ dIdx <- checkConstantNameAndType idx
  void $ checkMethodDesc dIdx
checkCPInfo_ (Constant_Module (ConstModule idx)) = do
  checkIsModule
  void $ checkModuleName idx
checkCPInfo_ (Constant_Package (ConstPackage idx)) = do
  checkIsModule
  void $ checkPackageName idx
checkCPInfo_ Constant_Invalid = return ()

checkConstantPoolInfo :: ClassFile -> MyErr ()
checkConstantPoolInfo cf =
  do
    let cp = constantPool cf
    let tags = cpTags cp
    let infos = cpEntries cp
    let cnt = cpCount cp
    when (length tags /= fromIntegral cnt || length tags /= length infos) $
      classFormatErr "Unexpected fatal error."
    when (cnt <= 0) $ classFormatErr "Constant pool count shoud greater than zero."
    when (head tags /= JVM_Constant_Invalid) $
      classFormatErr "Constant pool entry at zero is not invalid."
    runReaderT (doCheckCPInfo infos) (Env cf cp 0)

doCheckCPInfo :: [CPEntry] -> CPReader ()
doCheckCPInfo [] = return ()
doCheckCPInfo (x : xs) = do
  checkCPInfo x
  local incIdx $ doCheckCPInfo xs
  where
    incIdx (Env cf cp idx) = Env cf cp (idx + 1)

checkIsModule :: CPReader ()
checkIsModule = do
  cf <- asks envClassFile
  let flags = accessFlags cf
  let isModule = is_module flags
  unless isModule $ lift $ classFormatErr "Class File is not module, but contains a constant module info."

checkBootstrapAttrIdx :: U2 -> CPReader ()
checkBootstrapAttrIdx idx = do
  cf <- asks envClassFile
  let cnt = fromIntegral . length $ getBootstapMethods cf
  when (idx < 0 || idx >= cnt) $
    lift $
      classFormatErr $
        printf "Bootstrap attr count: %d, attr index: %d." cnt idx

checkConstantInvalid :: U2 -> CPReader ()
checkConstantInvalid idx = do
  cp <- asks envPool
  lift $ cpCheckTag JVM_Constant_Invalid cp idx

checkConstantUtf8 :: U2 -> CPReader ConstUtf8
checkConstantUtf8 idx = do
  cp <- asks envPool
  lift $ cpEntry cp idx

checkConstantClass :: U2 -> CPReader ConstUtf8
checkConstantClass idx = do
  cp <- asks envPool
  ConstClass nidx <- lift $ cpEntry cp idx
  utf8Checker verifyLegalClassName nidx

checkConstantMethodref :: U2 -> CPReader ConstMethodref
checkConstantMethodref idx = do
  cp <- asks envPool
  lift $ cpEntry cp idx

checkConstantMethodref_name :: U2 -> CPReader ConstUtf8
checkConstantMethodref_name idx = do
  ConstMethodref _ nt <- checkConstantMethodref idx
  ConstNameAndType n _ <- checkConstantNameAndType nt
  checkConstantUtf8 n

checkConstantInterfaceMethodref :: U2 -> CPReader ConstInterfaceMethodref
checkConstantInterfaceMethodref idx = do
  cp <- asks envPool
  lift $ cpEntry cp idx

checkConstantInterfaceMethodref_name :: U2 -> CPReader ConstUtf8
checkConstantInterfaceMethodref_name idx = do
  ConstInterfaceMethodref _ nt <- checkConstantInterfaceMethodref idx
  ConstNameAndType n _ <- checkConstantNameAndType nt
  checkConstantUtf8 n

checkConstantNameAndType :: U2 -> CPReader ConstNameAndType
checkConstantNameAndType idx = do
  cp <- asks envPool
  r@(ConstNameAndType nIdx dIdx) <- lift $ cpEntry cp idx
  _ <- checkConstantUtf8 nIdx
  _ <- checkConstantUtf8 dIdx
  return r

utf8Checker :: (Text -> MyErr Text) -> U2 -> CPReader ConstUtf8
utf8Checker checker idx = do
  cp <- asks envPool
  r@(ConstUtf8 name) <- lift $ cpEntry cp idx
  _ <- lift $ checker name
  return r

checkFieldDesc :: U2 -> CPReader ConstUtf8
checkFieldDesc = utf8Checker runParseFieldDescriptor

checkMethodDesc :: U2 -> CPReader ConstUtf8
checkMethodDesc = utf8Checker runParseMethodDescriptor

checkFieldName :: U2 -> CPReader ConstUtf8
checkFieldName = utf8Checker verifyLegalFieldName

checkMethodName :: U2 -> CPReader ConstUtf8
checkMethodName = utf8Checker verifyLegalMethodName

checkModuleName :: U2 -> CPReader ConstUtf8
checkModuleName = utf8Checker verifyLegalModuleName

checkPackageName :: U2 -> CPReader ConstUtf8
checkPackageName = utf8Checker verifyLegalClassName

data LegalTag = LegalClass | LegalField | LegalMethod deriving (Eq)

showErrMsg :: ParseError -> String
showErrMsg err =
  let msg = errorMessages err
   in messageString $ last msg

verifyLegalModuleName :: Text -> MyErr Text
verifyLegalModuleName name =
  case runP parseModuleName () "modulename" name of
    Left err -> classFormatErr $ showErrMsg err
    Right _ -> return name

verifyLegalClassName :: Text -> MyErr Text
verifyLegalClassName name
  | T.head name == jvm_signature_array = runParseFieldDescriptor name
  | verifyUnqualifiedName LegalClass name = return name
  | otherwise = classFormatErr $ printf "Illegal class name \"%s\"." name

verifyLegalFieldName :: Text -> MyErr Text
verifyLegalFieldName name
  | verifyUnqualifiedName LegalField name = return name
  | otherwise = classFormatErr $ printf "Illegal field name \"%s\"." name

verifyLegalMethodName :: Text -> MyErr Text
verifyLegalMethodName name
  | name == T.pack "<init>" || name == T.pack "<clinit>" = return name
  | verifyUnqualifiedName LegalMethod name = return name
  | otherwise = classFormatErr $ printf "Illegal method name \"%s\"." name

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

type TextParser = ParsecT Text () Identity

data FieldType = BaseType | ObjectType | ArrayType | VoidType

runParseFieldDescriptor :: Text -> MyErr Text
runParseFieldDescriptor name =
  case runP verifyFieldDescriptor () "fielddesc" name of
    Left err -> classFormatErr $ show err
    Right _ -> return name

runParseMethodDescriptor :: Text -> MyErr Text
runParseMethodDescriptor name =
  case runP verifyMethodDescriptor () "methoddesc" name of
    Left err -> classFormatErr $ show err
    Right _ -> return name

verifyMethodDescriptor :: TextParser ()
verifyMethodDescriptor = do
  _ <- char jvm_signature_func
  xs <- many verifyFieldType
  when (length xs > 255) $ fail "Method with over 255 parameters"
  _ <- char jvm_signature_endfunc
  verifyReturnDescriptor

verifyFieldDescriptor :: TextParser ()
verifyFieldDescriptor = verifyFieldType >> eof

verifyReturnDescriptor :: TextParser ()
verifyReturnDescriptor = do
  verifyFieldType <|> verifyVoidDescriptor

verifyVoidDescriptor :: TextParser ()
verifyVoidDescriptor = void (char jvm_signature_void)

verifyFieldType :: TextParser ()
verifyFieldType = verifyBaseType <|> verifyObjectType <|> verifyArrayType

verifyBaseType :: TextParser ()
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

verifyObjectType :: TextParser ()
verifyObjectType = do
  _ <- char jvm_signature_class
  name <- manyTill anyChar (char jvm_signature_endclass)
  case verifyLegalClassName $ T.pack name of
    Left _ -> fail $ printf "Illegal class name of ObjectType: %s." name
    Right _ -> return ()

verifyArrayType :: TextParser ()
verifyArrayType = do
  xs <- many1 $ char jvm_signature_array
  when (length xs > 255) $ fail $ "Array type with over 255 dimensions: " ++ show xs
  verifyFieldType
  return ()

parseModuleName :: TextParser [Char]
parseModuleName = do
  many1 parseModuleChar

parseModuleChar :: TextParser Char
parseModuleChar = do
  ch <- anyChar
  when (ch >= '\x0' && ch <= '\x001F') $ fail "Module name contain char below 0x001F."
  when (ch == ':' && ch == '@') $ fail "Char ':' or '@' in module name should be escaped."
  if ch == '\\'
    then char '\\' <|> char ':' <|> char '@' <?> "Expected escaped char '\\' ':' or '@'."
    else return ch
