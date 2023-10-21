module Pretty (ppClassFile) where

import ClassFile
import ClassFileParser (ClassFileParser (getClassName))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import Data.Text qualified as T
import Text.PrettyPrint (Doc, (<+>), (<>))
import Text.PrettyPrint qualified as PP
import Text.Printf (printf)
import Util (MyErr, U2, digitLength)
import Prelude hiding ((<>))

type CPReader = ReaderT Env (StateT MyState MyErr)

type MyState = U2

data Env = Env
  { envClassFile :: ClassFile,
    envConstPool :: ConstantPoolInfo
  }

ppClassFile :: ClassFile -> String
ppClassFile cf = do
  let cp = constantPool cf
  let initEnv = Env cf cp
  PP.render $ case evalStateT (runReaderT (ppr cf) initEnv) 0 of
    Left err -> PP.text $ "Prettyprint error: " ++ show err
    Right str -> str

class Pretty p where
  ppr :: p -> CPReader Doc

instance Pretty ClassFile where
  ppr _ = pprClassFile

lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => m a -> t1 (t2 m) a
lift2 a = lift $ lift a

hang :: Int -> [Doc] -> Doc
hang _ [] = PP.empty
hang n (x : xs) = PP.hang x n $ PP.vcat xs

pprClassFile :: CPReader Doc
pprClassFile = do
  cf <- asks envClassFile
  cp <- asks envConstPool
  let minor = minorVersion cf
  let major = majorVersion cf
  let cidx = thisClass cf
  cname <- lift2 $ getClassName cf
  let title = ppClassTitle cname
  let minorDoc = ppMinorVersion minor
  let majorDoc = ppMajorVersion major
  let thisClassDoc = ppThisClass cidx cname

  let classDoc = hang 2 [title, minorDoc, majorDoc, thisClassDoc]
  cpD <- ppr cp
  return $ PP.vcat [classDoc, cpD]

ppClassTitle :: ConstUtf8 -> Doc
ppClassTitle (ConstUtf8 name) = PP.text "public class" <+> PP.text (T.unpack name)

ppMinorVersion :: U2 -> Doc
ppMinorVersion n = PP.text "minor version: " <> PP.integer (fromIntegral n)

ppMajorVersion :: U2 -> Doc
ppMajorVersion n = PP.text "minor version: " <> PP.integer (fromIntegral n)

ppThisClass :: CPIndex -> ConstUtf8 -> Doc
ppThisClass idx (ConstUtf8 name) = PP.text "this_class: " <> ppRef idx <> ppComment name

instance Pretty ConstantPoolInfo where
  ppr _ = pprConstantPoolInfo

pprConstantPoolInfo :: CPReader Doc
pprConstantPoolInfo = do
  cp <- asks envConstPool
  let title = PP.text "Constant pool:"
  infosDoc <- mapM ppr $ cpInfos cp
  let cpDoc = hang 2 (title : infosDoc)
  return cpDoc

instance Pretty CPInfo where
  ppr = pprCPInfoWrap

pprCPInfoWrap :: CPInfo -> CPReader Doc
pprCPInfoWrap info = do
  val <- pprCPInfo info
  idx <-
    if PP.isEmpty val
      then return PP.empty
      else ppIndex
  incCPIdx
  return $ idx <> val

pprCPInfo :: CPInfo -> CPReader Doc
pprCPInfo Constant_Invalid = return PP.empty
pprCPInfo (Constant_Utf8 x) = ppr x
pprCPInfo (Constant_Integer x) = ppr x
pprCPInfo (Constant_Float x) = ppr x
pprCPInfo (Constant_Long x) = ppr x
pprCPInfo (Constant_Double x) = ppr x
pprCPInfo (Constant_Class x) = ppr x
pprCPInfo (Constant_String x) = ppr x
pprCPInfo (Constant_Fieldref x) = ppr x
pprCPInfo (Constant_Methodref x) = ppr x
pprCPInfo (Constant_InterfaceMethodref x) = ppr x
pprCPInfo (Constant_NameAndType x) = ppr x
pprCPInfo (Constant_MethodHandle x) = ppr x
pprCPInfo (Constant_MethodType x) = ppr x
pprCPInfo (Constant_Dynamic x) = ppr x
pprCPInfo (Constant_InvokeDynamic x) = ppr x
pprCPInfo (Constant_Module x) = ppr x
pprCPInfo (Constant_Package x) = ppr x

incCPIdx :: CPReader ()
incCPIdx = lift $ modify (+ 1)

alignStr :: Int -> String
alignStr = printf "%% %ds = "

ppIndex :: CPReader Doc
ppIndex = do
  idx <- lift get
  cp <- asks envConstPool
  let size = 1 + digitLength (cpCount cp)
  let val = printf (alignStr size) $ "#" ++ show idx
  return $ PP.sizedText 5 val

ppTag :: String -> Doc
ppTag str = PP.text $ printf "% -19s" str

ppRef :: U2 -> Doc
ppRef idx = PP.text "#" <> PP.int (fromIntegral idx)

ppComment :: T.Text -> Doc
ppComment s = PP.text "     // " <> PP.text (T.unpack s)

instance Pretty ConstUtf8 where
  ppr (ConstUtf8 val) = return $ ppTag "Utf8" <> PP.text (T.unpack val)

instance Pretty ConstInteger where
  ppr (ConstInteger val) = return $ ppTag "Integer" <> PP.integer (fromIntegral val)

instance Pretty ConstFloat where
  ppr (ConstFloat val) = return $ ppTag "Float" <> PP.float val

instance Pretty ConstLong where
  ppr (ConstLong val) = return $ ppTag "Long" <> PP.integer (fromIntegral val)

instance Pretty ConstDouble where
  ppr (ConstDouble val) = return $ ppTag "Double" <> PP.double val

instance Pretty ConstClass where
  ppr (ConstClass val) = return $ ppTag "Class" <> ppRef val

instance Pretty ConstString where
  ppr (ConstString val) = return $ ppTag "String" <> ppRef val

instance Pretty ConstFieldref where
  ppr (ConstFieldref cIdx ntIdx) =
    return $
      ppTag "Fieldref"
        <> ppRef cIdx
        <> PP.char '.'
        <> ppRef ntIdx

instance Pretty ConstMethodref where
  ppr (ConstMethodref cIdx ntIdx) =
    return $
      ppTag "Methodref"
        <> ppRef cIdx
        <> PP.char '.'
        <> ppRef ntIdx

instance Pretty ConstInterfaceMethodref where
  ppr (ConstInterfaceMethodref cIdx ntIdx) =
    return $
      ppTag "InterfaceMethodref"
        <> ppRef cIdx
        <> PP.char '.'
        <> ppRef ntIdx

instance Pretty ConstNameAndType where
  ppr (ConstNameAndType nIdx dIdx) =
    return $
      ppTag "NameAndType"
        <> ppRef nIdx
        <> PP.char ':'
        <> ppRef dIdx

instance Pretty ConstMethodHandle where
  ppr (ConstMethodHandle kind idx) =
    return $
      ppTag "MethodHandle"
        <> PP.text (show kind)
        <+> ppRef idx

instance Pretty ConstMethodType where
  ppr (ConstMethodType dIdx) = return $ ppTag "MethodType" <> ppRef dIdx

instance Pretty ConstDynamic where
  ppr (ConstDynamic attrIdx ntIdx) =
    return $
      ppTag "Dynamic"
        <> PP.text "attr"
        <> ppRef attrIdx
        <+> ppRef ntIdx

instance Pretty ConstInvokeDynamic where
  ppr (ConstInvokeDynamic attrIdx ntIdx) =
    return $
      ppTag "InvokeDynamic"
        <> PP.text "attr"
        <> ppRef attrIdx
        <+> ppRef ntIdx

instance Pretty ConstModule where
  ppr (ConstModule nIdx) = return $ ppTag "Module" <> ppRef nIdx

instance Pretty ConstPackage where
  ppr (ConstPackage nIdx) = return $ ppTag "Package" <> ppRef nIdx