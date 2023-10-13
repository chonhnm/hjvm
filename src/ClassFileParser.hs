module ClassFileParser where

import ClassFile
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT (runReaderT), runReader, ask, local)
import Data.Foldable (forM_)
import Data.Functor.Identity (Identity (Identity))
import Data.Text (Text)
import Data.Text qualified as T
import Text.Parsec
import Text.Printf (printf)
import Util
import Control.Monad.Trans.Class (MonadTrans(lift))

class ClassFileParser cf where
  getMajorVersion :: cf -> U2
  getCPInfo :: cf -> [CPInfo]

instance ClassFileParser ClassFile where
  getMajorVersion = majorVersion
  getCPInfo = constantPool

type ClassFileEnv = ReaderT ClassFile (ExceptT AppErr Identity)

class CPInfoChecker a where
  checkCPInfo :: ClassFile -> a -> MyErr ()

instance CPInfoChecker CPInfo where
  checkCPInfo = checkCPInfo_

checkCPInfo_ :: ClassFile -> CPInfo -> MyErr ()
checkCPInfo_ cf (Constant_Utf8 _) = undefined
checkCPInfo_ cf (Constant_Integer _) = undefined
checkCPInfo_ cf (Constant_Float _) = undefined
checkCPInfo_ cf (Constant_Long _) = undefined
checkCPInfo_ cf (Constant_Double _) = undefined
checkCPInfo_ cf (Constant_Class idx) = checkClassName cf idx
checkCPInfo_ cf (Constant_String idx) = undefined
checkCPInfo_ cf (Constant_Fieldref cIdx ntIdx) = do
  checkClassName cf cIdx
  ConstNameAndType nIdx desIdx <- cpNameAndType (getCPInfo cf) ntIdx
  checkFieldName cf nIdx
  checkFieldDesc cf desIdx
checkCPInfo_ cf (Constant_Methodref cIdx ntIdx) = do
  checkClassName cf cIdx
  ConstNameAndType nIdx desIdx <- cpNameAndType (getCPInfo cf) ntIdx
  checkMethodName cf nIdx
  checkMethodDesc (T.pack "") cf desIdx
checkCPInfo_ cf (Constant_InterfaceMethodref cIdx ntIdx) = undefined
checkCPInfo_ cf (Constant_NameAndType nt) = undefined
checkCPInfo_ cf (Constant_MethodHandle kind idx) = undefined
checkCPInfo_ cf (Constant_MethodType idx) = undefined
checkCPInfo_ cf (Constant_Dynamic attrIdx idx) = undefined
checkCPInfo_ cf (Constant_InvokeDynamic attrIdx idx) = undefined
checkCPInfo_ cf (Constant_Module idx) = undefined
checkCPInfo_ cf (Constant_Package idx) = undefined

checkFieldDesc :: ClassFile -> U2 -> MyErr ()
checkFieldDesc = checkUtf8 runParseFieldDescriptor

checkMethodDesc :: Text -> ClassFile -> U2 -> MyErr ()
checkMethodDesc name = checkUtf8 runParseMethodDescriptor

checkUtf8 :: (Text -> Maybe String) -> ClassFile -> U2 -> MyErr ()
checkUtf8 checker cf idx = do
  let cp = getCPInfo cf
  name <- cpUtf8 cp idx
  case checker name of
    Nothing -> Right ()
    Just str -> Left $ ClassFormatError str

checkFieldName :: ClassFile -> U2 -> MyErr ()
checkFieldName = checkUtf8 verifyLegalFieldName

checkMethodName :: ClassFile -> U2 -> MyErr ()
checkMethodName = checkUtf8 verifyLegalMethodName

checkClassName :: ClassFile -> U2 -> MyErr ()
checkClassName = checkUtf8 verifyLegalClassName

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

data MyState = MS {array_dims :: Int, param_count :: Int}

emptyMyState :: MyState
emptyMyState = MS 0 0

type FieldReader = ReaderT MyState Identity

type FieldParser = ParsecT Text () FieldReader

data FieldType = BaseType | ObjectType | ArrayType

data VoidType = VoidType

runParseFieldDescriptor :: Text -> Maybe String
runParseFieldDescriptor name =
  case runReader
    (runPT verifyFieldDescriptor () "fielddesc" name)
    emptyMyState of
    Left err -> Just $ show err
    Right _ -> Nothing

runParseMethodDescriptor :: Text -> Maybe String
runParseMethodDescriptor name =
  case runReader
    (runPT verifyMethodDescriptor () "methoddesc" name)
    emptyMyState of
    Left err -> Just $ show err
    Right _ -> Nothing

verifyMethodDescriptor :: FieldParser ()
verifyMethodDescriptor = do
  _ <- char jvm_signature_func
  _ <- many verifyFieldType
  _ <- char jvm_signature_endfunc
  verifyReturnDescriptor

verifyFieldDescriptor :: FieldParser ()
verifyFieldDescriptor = verifyFieldType >> eof

verifyReturnDescriptor :: FieldParser ()
verifyReturnDescriptor = do
  verifyFieldType <|> verifyVoidDescriptor

verifyVoidDescriptor :: FieldParser ()
verifyVoidDescriptor = char jvm_signature_void >> return ()

verifyFieldType :: FieldParser ()
verifyFieldType = do
  MS dims _ <- lift ask
  if dims > 255
    then fail "Array type with over 255 dimensions."
    else verifyBaseType <|> verifyObjectType <|> verifyArrayType

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
  _ <- char jvm_signature_array
  _ <- lift $ local (\(MS dims pc) -> MS (dims + 1) pc) verifyFieldType
  return ()