module ClassFileParser where

import ClassFile
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (forM_)
import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import Text.Parsec
import Text.Printf (printf)
import Util

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
  checkCPInfo cf (Constant_Class idx) = do
    let cp = getCPInfo cf
    name <- cpUtf8 cp idx
    case verifyLegalClassName name of
      Nothing -> Right ()
      Just str -> Left $ ClassFormatError str
  checkCPInfo _ _ = undefined

-- checkCPInfo cf (Constant_Utf8 _) = undefined
-- checkCPInfo cf (Constant_Integer _) = undefined
-- checkCPInfo cf (Constant_Float ) = undefined
-- checkCPInfo cf (Constant_Long ) = undefined
-- checkCPInfo cf (Constant_Double ) = undefined
-- checkCPInfo cf (Constant_Class ) = undefined
-- checkCPInfo cf (Constant_String idx) = undefined
-- checkCPInfo cf (Constant_Fieldref cIdx ntIdx) = undefined
-- checkCPInfo cf (Constant_Methodref cIdx ntIdx) = undefined
-- checkCPInfo cf (Constant_InterfaceMethodref cIdx ntIdx) = undefined
-- checkCPInfo cf (Constant_NameAndType ) = undefined
-- checkCPInfo cf (Constant_MethodHandle ) = undefined
-- checkCPInfo cf (Constant_MethodType ) = undefined
-- checkCPInfo cf (Constant_Dynamic ) = undefined
-- checkCPInfo cf (Constant_InvokeDynamic ) = undefined
-- checkCPInfo cf (Constant_Module ) = undefined
-- checkCPInfo cf (Constant_Package  ) = undefined

data LegalTag = LegalClass | LegalField | LegalMethod deriving (Eq)

verifyLegalClassName :: T.Text -> Maybe String
verifyLegalClassName name
  | T.head name == jvm_signature_array = runParseFieldDescriptor name
  | verifyUnqualifiedName LegalClass name = Nothing
  | otherwise = Just $ printf "Illegal class name \"%s\"." name

verifyLegalFieldName :: T.Text -> Bool
verifyLegalFieldName = verifyUnqualifiedName LegalField

verifyLegalMethodName :: T.Text -> Bool
verifyLegalMethodName name =
  let ch = T.head name
   in if ch == jvm_signature_special
        then name == T.pack "<init>" || name == T.pack "<clinit>"
        else verifyUnqualifiedName LegalMethod name

verifyUnqualifiedName :: LegalTag -> T.Text -> Bool
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

type MyParser = ParsecT T.Text MyState Identity

newtype MyState = MS {array_dims :: Int}

emptyMyState :: MyState
emptyMyState = MS 0

data FieldType = BaseType | ObjectType | ArrayType

parseFieldDescriptor :: MyParser ()
parseFieldDescriptor = parseFieldType >> eof

runParseFieldDescriptor :: T.Text -> Maybe String
runParseFieldDescriptor name = case runP parseFieldDescriptor emptyMyState "fieldtype" name of
  Left err -> Just $ show err
  Right _ -> Nothing

parseFieldType :: MyParser ()
parseFieldType = do
  MS dims <- getState
  if dims > 255
    then fail "Array type with over 255 dimensions."
    else parseBaseType <|> parseObjectType <|> parseArrayType

parseBaseType :: MyParser ()
parseBaseType = do
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

parseObjectType :: MyParser ()
parseObjectType = do
  _ <- char jvm_signature_class
  name <- manyTill anyChar (char jvm_signature_endclass)
  forM_ (verifyLegalClassName $ T.pack name) fail

parseArrayType :: MyParser ()
parseArrayType = do
  _ <- char jvm_signature_array
  modifyState (\(MS dims) -> MS (dims + 1))
  parseFieldType
  return ()