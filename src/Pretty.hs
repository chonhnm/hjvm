module Pretty (ppClassFile) where

import AccessFlags (IAccessFlags (encodeFlags, encodeHex))
import ClassFile
import ClassFileParser (ClassFileParser (getClassName, getSuperClassName, getThisClassName, getUtf8Name))
import ConstantPool
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import Data.Text qualified as T
import Text.PrettyPrint (Doc, ($$), (<+>), (<>))
import Text.PrettyPrint qualified as PP
import Text.Printf (printf)
import Util (MyErr, U2, digitLength)
import Prelude hiding ((<>))

indentOfSubItem :: Int
indentOfSubItem = 2

indentOfRef :: Int
indentOfRef = 19

indentOfComment :: Int
indentOfComment = 15

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
  PP.render $ case evalStateT (runReaderT pprClassFile initEnv) 0 of
    Left err -> PP.text $ "Prettyprint error: " ++ show err
    Right str -> str

class Pretty p where
  ppr :: p -> CPReader Doc

lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => m a -> t1 (t2 m) a
lift2 a = lift $ lift a

hang :: Int -> [Doc] -> Doc
hang _ [] = PP.empty
hang n (x : xs) = PP.hang x n $ PP.vcat xs

pprClassFile :: CPReader Doc
pprClassFile = do
  subDoc <-
    sequence
      [ pprClassTitle,
        pprMinorVersion,
        pprMajorVersion,
        pprFlags,
        pprThisClass,
        pprSuperClass,
        pprIFMACounts
      ]
  let classDoc = hang indentOfSubItem subDoc
  cpDoc <- pprConstantPoolInfo
  return $ PP.vcat [classDoc, cpDoc]

pprClassTitle :: CPReader Doc
pprClassTitle = do
  cf <- asks envClassFile
  ConstUtf8 name <- lift2 $ getThisClassName cf
  return $ PP.text "public class" <+> PP.text (T.unpack name)

pprMinorVersion :: CPReader Doc
pprMinorVersion = do
  cf <- asks envClassFile
  let n = cf.minorVersion
  return $ PP.text "minor version: " <> PP.integer (fromIntegral n)

pprMajorVersion :: CPReader Doc
pprMajorVersion = do
  cf <- asks envClassFile
  let n = cf.majorVersion
  return $ PP.text "major version: " <> PP.integer (fromIntegral n)

pprFlags :: CPReader Doc
pprFlags = do
  cf <- asks envClassFile
  let flags = cf.accessFlags
  let enHex = PP.text $ encodeHex flags
  let enFlags = PP.punctuate PP.comma $ map (PP.text . show) $ encodeFlags flags

  return $ PP.text "flags:" <+> PP.parens enHex <+> PP.hsep enFlags

pprThisClass :: CPReader Doc
pprThisClass = do
  cf <- asks envClassFile
  let idx = cf.thisClass
  ConstUtf8 name <- lift2 $ getClassName cf idx
  return $ PP.text "this_class: " <> ppRef idx $$ ppComment name

pprSuperClass :: CPReader Doc
pprSuperClass = do
  cf <- asks envClassFile
  ConstUtf8 name <- lift2 $ getSuperClassName cf
  return $ PP.text "super_class: " <> ppRef cf.superClass $$ ppComment name

pprIFMACounts :: CPReader Doc
pprIFMACounts = do
  cf <- asks envClassFile
  let ic = PP.text "interfaces:" <+> PP.int (length cf.interfaces)
  let fc = PP.text "fields:" <+> PP.int (length cf.fields)
  let mc = PP.text "methods:" <+> PP.int (length cf.methods)
  let ac = PP.text "attributes:" <+> PP.int (length cf.attributes)
  let cnts = PP.punctuate PP.comma [ic, fc, mc, ac]
  return $ PP.hsep cnts

pprConstantPoolInfo :: CPReader Doc
pprConstantPoolInfo = do
  cp <- asks envConstPool
  let title = PP.text "Constant pool:"
  infosDoc <- mapM ppr $ cpEntries cp
  let cpDoc = hang indentOfSubItem (title : infosDoc)
  return cpDoc

instance Pretty CPEntry where
  ppr = pprCPInfoWrap

pprCPInfoWrap :: CPEntry -> CPReader Doc
pprCPInfoWrap info = do
  val <- pprCPInfo info
  idx <-
    if PP.isEmpty val
      then return PP.empty
      else ppIndex
  incCPIdx
  return $ idx <+> val

pprCPInfo :: CPEntry -> CPReader Doc
pprCPInfo (Constant_Invalid x) = ppr x 
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
alignStr = printf "%% %ds ="

ppIndex :: CPReader Doc
ppIndex = do
  idx <- lift get
  cp <- asks envConstPool
  let size = 1 + digitLength (cpCount cp)
  let val = printf (alignStr size) $ "#" ++ show idx
  return $ PP.text val

ppTag :: String -> Doc -> Doc
ppTag str doc = PP.text str $$ PP.nest indentOfRef doc

ppRef :: U2 -> Doc
ppRef idx = PP.text "#" <> PP.int (fromIntegral idx)

ppComment :: T.Text -> Doc
ppComment s = PP.nest indentOfComment $ PP.text "//" <+> PP.text (T.unpack s)

instance Pretty ConstInvalid where
  ppr _ = return PP.empty

instance Pretty ConstUtf8 where
  ppr (ConstUtf8 val) = return $ ppTag "Utf8" $ PP.text (T.unpack val)

instance Pretty ConstInteger where
  ppr (ConstInteger val) = return $ ppTag "Integer" $ PP.integer (fromIntegral val)

instance Pretty ConstFloat where
  ppr (ConstFloat val) = return $ ppTag "Float" $ PP.float val

instance Pretty ConstLong where
  ppr (ConstLong val) = return $ ppTag "Long" $ PP.integer (fromIntegral val)

instance Pretty ConstDouble where
  ppr (ConstDouble val) = return $ ppTag "Double" $ PP.double val

instance Pretty ConstClass where
  ppr (ConstClass nIdx) = do
    cf <- asks envClassFile
    ConstUtf8 name <- lift2 $ getUtf8Name cf nIdx
    return $ ppTag "Class" $ ppRef nIdx $$ ppComment name

instance Pretty ConstString where
  ppr (ConstString val) = return $ ppTag "String" $ ppRef val

instance Pretty ConstFieldref where
  ppr (ConstFieldref cIdx ntIdx) = do
    ConstUtf8 name <- getFieldOrMethodRefName cIdx ntIdx
    return $
      ppTag "Fieldref" $
        ppRef cIdx
          <> PP.char '.'
          <> ppRef ntIdx
          $$ ppComment name

instance Pretty ConstMethodref where
  ppr (ConstMethodref cIdx ntIdx) = do
    ConstUtf8 name <- getFieldOrMethodRefName cIdx ntIdx
    return $
      ppTag "Methodref" $
        ppRef cIdx
          <> PP.char '.'
          <> ppRef ntIdx
          $$ ppComment name

instance Pretty ConstInterfaceMethodref where
  ppr (ConstInterfaceMethodref cIdx ntIdx) = do
    ConstUtf8 name <- getFieldOrMethodRefName cIdx ntIdx
    return $
      ppTag "InterfaceMethodref" $
        ppRef cIdx
          <> PP.char '.'
          <> ppRef ntIdx
          $$ ppComment name

instance Pretty ConstNameAndType where
  ppr (ConstNameAndType nIdx dIdx) = do
    cf <- asks envClassFile
    ConstUtf8 name <- lift2 $ getNameAndTypeUnwraped cf nIdx dIdx
    return $
      ppTag "NameAndType" $
        ppRef nIdx
          <> PP.char ':'
          <> ppRef dIdx
          $$ ppComment name

instance Pretty ConstMethodHandle where
  ppr (ConstMethodHandle kind idx) =
    return $
      ppTag "MethodHandle" $
        PP.text (show kind)
          <+> ppRef idx

instance Pretty ConstMethodType where
  ppr (ConstMethodType dIdx) = return $ ppTag "MethodType" $ ppRef dIdx

instance Pretty ConstDynamic where
  ppr (ConstDynamic attrIdx ntIdx) =
    return $
      ppTag "Dynamic" $
        PP.text "attr"
          <> ppRef attrIdx
          <+> ppRef ntIdx

instance Pretty ConstInvokeDynamic where
  ppr (ConstInvokeDynamic attrIdx ntIdx) =
    return $
      ppTag "InvokeDynamic" $
        PP.text "attr"
          <> ppRef attrIdx
          <+> ppRef ntIdx

instance Pretty ConstModule where
  ppr (ConstModule nIdx) = return $ ppTag "Module" $ ppRef nIdx

instance Pretty ConstPackage where
  ppr (ConstPackage nIdx) = return $ ppTag "Package" $ ppRef nIdx

getNameAndType :: ClassFile -> U2 -> MyErr ConstUtf8
getNameAndType cf idx = do
  let cp = cf.constantPool
  (ConstNameAndType nIdx dIdx) <- cpEntry cp idx
  getNameAndTypeUnwraped cf nIdx dIdx

getNameAndTypeUnwraped :: ClassFile -> U2 -> U2 -> MyErr ConstUtf8
getNameAndTypeUnwraped cf nameIdx descIdx = do
  ConstUtf8 name <- getUtf8Name cf nameIdx
  ConstUtf8 dname <- getUtf8Name cf descIdx
  return $ ConstUtf8 $ name `T.append` ":" `T.append` dname

getFieldOrMethodRefName :: U2 -> U2 -> CPReader ConstUtf8
getFieldOrMethodRefName cIdx ntIdx = do
  cf <- asks envClassFile
  ConstUtf8 cname <- lift2 $ getClassName cf cIdx
  ConstUtf8 ntname <- lift2 $ getNameAndType cf ntIdx
  let mrname = cname `T.append` "." `T.append` ntname
  return $ ConstUtf8 mrname
