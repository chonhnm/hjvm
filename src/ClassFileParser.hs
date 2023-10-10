module ClassFileParser where

import ClassFile
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader (ReaderT)
import Data.Functor.Identity (Identity)
import Data.Text qualified as T
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
    ConstantUtf8 val <- cpUtf8 cp idx
    if verifyLegalClassName val
      then return ()
      else Left $ ClassFormatError $ T.pack (printf "Illegal class name \"%s\"." val)
  checkCPInfo _ _ = Left $ PE NonExausted

data LegalTag = LegalClass | LegalField | LegalMethod deriving (Eq)

verifyLegalClassName :: T.Text -> Bool
verifyLegalClassName = verifyUnqualifiedName LegalClass

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