module Pretty(ppClassFile) where

import ClassFile
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReader)
import Data.Functor.Identity (Identity)
import Data.Text qualified as T
import Text.PrettyPrint (Doc, ($$), (<+>), (<>))
import Text.PrettyPrint qualified as PP
import Util (U2)
import Prelude hiding ((<>))

type CPReader = ReaderT Env Identity

data Env = Env
  { envClassFile :: ClassFile,
    envPool :: ConstantPoolInfo,
    envCurPoolIdx :: U2
  }

ppClassFile :: ClassFile -> String
ppClassFile cf = do
  let cp = constantPool cf
  let initEnv = Env cf cp 0
  PP.render $ runReader (ppr cp) initEnv

class Pretty p where
  ppr :: p -> CPReader Doc

instance Pretty ClassFile where
  ppr _ = pprClassFile

pprClassFile :: CPReader Doc
pprClassFile = do
  cf <- asks envClassFile
  let minor = minorVersion cf
  let major = majorVersion cf
  let title = PP.text "public class"
  let minorD = PP.text "minor version: " <> PP.integer (fromIntegral minor)
  let majorD = PP.text "major version: " <> PP.integer (fromIntegral major)
  cp <- asks envPool
  cpD <- ppr cp
  return $ title $$ minorD $$ majorD $$ cpD

instance Pretty ConstantPoolInfo where
  ppr _ = pprConstantPoolInfo

pprConstantPoolInfo :: CPReader Doc
pprConstantPoolInfo = do
  cp <- asks envPool
  let infos = cpInfos cp
  let title = PP.text "Constant pool:\n"
  (title $$) <$> pprCPInfos infos

pprCPInfos :: [CPInfo] -> CPReader Doc
pprCPInfos [] = return PP.empty
pprCPInfos (x : xs) = do
  idx <- asks envCurPoolIdx
  doc <- (ppIdx idx <>) <$> pprCPInfo x
  doc2 <- local incIdx $ pprCPInfos xs
  return $ doc $$ doc2
  where
    incIdx (Env cf cp idx) = Env cf cp (idx + 1)

instance Pretty CPInfo where
  ppr = pprCPInfo

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

ppIdx :: U2 -> Doc
ppIdx idx = PP.sizedText 5 ("#" ++ show idx)

ppTag :: String -> Doc
ppTag = PP.sizedText 19

ppRef :: U2 -> Doc
ppRef idx = PP.text "#" <> PP.int (fromIntegral idx)

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