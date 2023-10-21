module Pretty (ppClassFile) where

import ClassFile
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), asks)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import Data.Text as T
import Prettyprinter ((<+>), (<>))
import Prettyprinter qualified as PP
import Text.Printf (printf)
import Util (MyErr, U2)
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
  show $ case evalStateT (runReaderT (ppr cf) initEnv) 0 of
    Left err -> "Prettyprint error: " <> PP.pretty err
    Right str -> str

type Doc = PP.Doc Anno
data Anno = Emp | NonEmp  

class Pretty p where
  ppr :: p -> CPReader Doc

instance Pretty ClassFile where
  ppr _ = pprClassFile

lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) => m a -> t1 (t2 m) a
lift2 a = lift $ lift a

pprClassFile :: CPReader Doc
pprClassFile = do
  cf <- asks envClassFile
  cp <- asks envConstPool
  let minor = minorVersion cf
  let major = majorVersion cf
  ConstUtf8 cname <- lift2 $ do
    ConstClass nidx <- cpConstClass cp cf.thisClass
    cpConstUtf8 cp nidx
  let title = "public class" <+> PP.pretty cname
  let minorD = "minor version: " <> PP.pretty minor
  let majorD = "major version: " <> PP.pretty major
  cpD <- ppr cp
  return $ PP.nest 4 $ PP.vsep [title, minorD, majorD, cpD]

instance Pretty ConstantPoolInfo where
  ppr _ = pprConstantPoolInfo

pprConstantPoolInfo :: CPReader Doc
pprConstantPoolInfo = do
  cp <- asks envConstPool
  let title = "Constant pool:"
  let infos = cpInfos cp
  infosD <- PP.vcat <$> mapM ppr infos
  return $ PP.nest 4 $ PP.vsep [title, infosD]

instance Pretty CPInfo where
  ppr = pprCPInfoWrap

pprCPInfoWrap :: CPInfo -> CPReader Doc
pprCPInfoWrap info = do
  val <- pprCPInfo info
  idx <-
    case val of
      -- _ -> return val
      _ -> ppIndex
  incCPIdx
  return $ idx <> val

pprCPInfo :: CPInfo -> CPReader Doc
pprCPInfo Constant_Invalid = return PP.emptyDoc
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

ppIndex :: CPReader Doc
ppIndex = do
  idx <- lift get
  let val = T.pack $ printf "% 5s = " ("#" ++ show idx)
  return $ PP.pretty val

ppTag :: String -> Doc
ppTag str = PP.pretty $ T.pack $ printf "% -19s" str

ppRef :: U2 -> Doc
ppRef idx = "#" <> PP.pretty idx

instance Pretty ConstUtf8 where
  ppr (ConstUtf8 val) = return $ ppTag "Utf8" <> PP.pretty val

instance Pretty ConstInteger where
  ppr (ConstInteger val) = return $ ppTag "Integer" <> PP.pretty val

instance Pretty ConstFloat where
  ppr (ConstFloat val) = return $ ppTag "Float" <> PP.pretty val

instance Pretty ConstLong where
  ppr (ConstLong val) = return $ ppTag "Long" <> PP.pretty val

instance Pretty ConstDouble where
  ppr (ConstDouble val) = return $ ppTag "Double" <> PP.pretty val

instance Pretty ConstClass where
  ppr (ConstClass val) = return $ ppTag "Class" <> ppRef val

instance Pretty ConstString where
  ppr (ConstString val) = return $ ppTag "String" <> ppRef val

instance Pretty ConstFieldref where
  ppr (ConstFieldref cIdx ntIdx) =
    return $
      ppTag "Fieldref"
        <> ppRef cIdx
        <> PP.dot
        <> ppRef ntIdx

instance Pretty ConstMethodref where
  ppr (ConstMethodref cIdx ntIdx) =
    return $
      ppTag "Methodref"
        <> ppRef cIdx
        <> PP.dot
        <> ppRef ntIdx

instance Pretty ConstInterfaceMethodref where
  ppr (ConstInterfaceMethodref cIdx ntIdx) =
    return $
      ppTag "InterfaceMethodref"
        <> ppRef cIdx
        <> PP.dot
        <> ppRef ntIdx

instance Pretty ConstNameAndType where
  ppr (ConstNameAndType nIdx dIdx) =
    return $
      ppTag "NameAndType"
        <> ppRef nIdx
        <> PP.colon
        <> ppRef dIdx

instance Pretty ConstMethodHandle where
  ppr (ConstMethodHandle kind idx) =
    return $
      ppTag "MethodHandle"
        <> PP.pretty (T.pack $ show kind)
        <+> ppRef idx

instance Pretty ConstMethodType where
  ppr (ConstMethodType dIdx) = return $ ppTag "MethodType" <> ppRef dIdx

instance Pretty ConstDynamic where
  ppr (ConstDynamic attrIdx ntIdx) =
    return $
      ppTag "Dynamic"
        <> "attr"
        <> ppRef attrIdx
        <+> ppRef ntIdx

instance Pretty ConstInvokeDynamic where
  ppr (ConstInvokeDynamic attrIdx ntIdx) =
    return $
      ppTag "InvokeDynamic"
        <> "attr"
        <> ppRef attrIdx
        <+> ppRef ntIdx

instance Pretty ConstModule where
  ppr (ConstModule nIdx) = return $ ppTag "Module" <> ppRef nIdx

instance Pretty ConstPackage where
  ppr (ConstPackage nIdx) = return $ ppTag "Package" <> ppRef nIdx