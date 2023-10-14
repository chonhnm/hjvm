module ClassFileParser where

import ClassFile
import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
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
checkCPInfo_  (Constant_Utf8 _) = undefined
checkCPInfo_  (Constant_Integer _) = undefined
checkCPInfo_  (Constant_Float _) = undefined
checkCPInfo_  (Constant_Long _) = undefined
checkCPInfo_  (Constant_Double _) = undefined
checkCPInfo_  (Constant_Class idx) = do
  _ <- getAndCheckClassName cf idx
  return ()
checkCPInfo_  (Constant_String idx) = do
  _ <- cpUtf8 (getConstantPoolInfo cf) idx
  return ()
checkCPInfo_  (Constant_Fieldref cIdx ntIdx) = do
  _ <- getAndCheckClassName cf cIdx
  ConstNameAndType nIdx desIdx <- cpNameAndType (getConstantPoolInfo cf) ntIdx
  _ <- getAndCheckFieldName cf nIdx
  _ <- getAndCheckFieldDesc cf desIdx
  return ()
checkCPInfo_  (Constant_Methodref cIdx ntIdx) = do
  _ <- getAndCheckClassName cf cIdx
  ConstNameAndType nIdx desIdx <- cpNameAndType (getConstantPoolInfo cf) ntIdx
  name <- getAndCheckMethodName cf nIdx
  desc <- getAndCheckMethodDesc cf desIdx
  when (T.head name == jvm_signature_special)
    $ when
      (name /= T.pack "<init>" || T.last desc /= jvm_signature_void)
    $ Left
    $ ClassFormatError
    $ printf "Unexpected method name and type: %s:%s" name desc
checkCPInfo_  (Constant_InterfaceMethodref cIdx ntIdx) = do
  _ <- getAndCheckClassName cf cIdx
  ConstNameAndType nIdx desIdx <- cpNameAndType (getConstantPoolInfo cf) ntIdx
  name <- getAndCheckMethodName cf nIdx
  _ <- getAndCheckMethodDesc cf desIdx
  when (T.head name == jvm_signature_special) $
    Left $
      ClassFormatError $
        printf "Unexpected interface method name and type: %s:%s" name
checkCPInfo_  (Constant_NameAndType nt) = undefined
checkCPInfo_  (Constant_MethodHandle kind idx) = undefined
checkCPInfo_  (Constant_MethodType idx) = undefined
checkCPInfo_  (Constant_Dynamic attrIdx idx) = undefined
checkCPInfo_  (Constant_InvokeDynamic attrIdx idx) = undefined
checkCPInfo_  (Constant_Module idx) = undefined
checkCPInfo_  (Constant_Package idx) = undefined
checkCPInfo_ Constant_Invalid = undefined

type CPReader = ReaderT ConstantPoolInfo MyErr

cfErr :: String -> Either AppErr b
cfErr str = Left $ ClassFormatError str

data Env = Env {env_cp :: ConstantPoolInfo, env_tags :: [CPTag]}

checkConstantPoolInfo :: ConstantPoolInfo -> MyErr ()
checkConstantPoolInfo cp =
  do
    let tags = cpTags cp
    let infos = cpInfos cp
    let cnt = cpCount cp
    when (length tags /= fromIntegral cnt || length tags /= length infos) $
      cfErr "Unexpected fatal error."
    when (cnt <= 0) $ cfErr "Constant pool count shoud greater than zero."
    when (head tags /= JVM_Constant_Invalid || head infos /= Constant_Invalid) $
      cfErr "Constant pool entry at zero is not invalid."
    runReaderT (doCheckCPInfo infos 0) cp

doCheckCPInfo :: [CPInfo] -> U2 -> CPReader ()
doCheckCPInfo [] _ = return ()
doCheckCPInfo (x : xs) idx = do
  doCheckOne x idx
  doCheckCPInfo xs (idx + 1)

doCheckOne :: CPInfo -> U2 -> CPReader ()
doCheckOne tag idx = undefined

checkConstantUtf8 :: CPReader ()
checkConstantUtf8 = undefined

checkConstantInteger :: CPReader ()
checkConstantInteger = undefined

checkConstantFloat :: CPReader ()
checkConstantFloat = undefined

checkConstantLong :: CPReader ()
checkConstantLong = undefined

checkConstantDouble :: CPReader ()
checkConstantDouble = undefined

checkConstantClass :: U2 -> CPReader Text
checkConstantClass = checkUtf8 verifyLegalClassName 

checkConstantString :: CPReader ()
checkConstantString = undefined

checkConstantFieldref :: U2 -> CPReader ()
checkConstantFieldref idx = do 
  _ <- checkConstantClass idx 
  ConstNameAndType nIdx desIdx <- cpNameAndType (getConstantPoolInfo cf) ntIdx
  _ <- getAndCheckFieldName cf nIdx
  _ <- getAndCheckFieldDesc cf desIdx
  return ()

checkConstantMethodref :: CPReader ()
checkConstantMethodref = undefined

checkConstantInterfaceMethodref :: CPReader ()
checkConstantInterfaceMethodref = undefined

checkConstantNameAndType :: CPReader ()
checkConstantNameAndType = undefined

checkConstantMethodHandle :: CPReader ()
checkConstantMethodHandle = undefined

checkConstantMethodType :: CPReader ()
checkConstantMethodType = undefined

checkConstantDynamic :: CPReader ()
checkConstantDynamic = undefined

checkConstantInvokeDynamic :: CPReader ()
checkConstantInvokeDynamic = undefined

checkConstantModule :: CPReader ()
checkConstantModule = undefined

checkConstantPackage :: CPReader ()
checkConstantPackage = undefined

--------------------------
checkUtf8 :: (Text -> Maybe String) -> U2 -> CPReader Text
checkUtf8 checker idx = do
  let cp = ask
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

getAndCheckClassName :: U2 -> CPReader Text
getAndCheckClassName = checkUtf8 verifyLegalClassName

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