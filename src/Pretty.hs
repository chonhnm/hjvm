module Pretty where

import ClassFile
import Text.PrettyPrint (Doc, (<+>), (<>))
import Text.PrettyPrint qualified as PP

class Pretty p where
  ppr :: p -> Doc

instance Pretty CPInfo where
  ppr = pprCPInfo

pprCPInfo :: CPInfo -> Doc
pprCPInfo Constant_Invalid = PP.empty
pprCPInfo (Constant_Utf8 _) = undefined
pprCPInfo (Constant_Integer _) = undefined
pprCPInfo (Constant_Float _) = undefined
pprCPInfo (Constant_Long _) = undefined
pprCPInfo (Constant_Double _) = undefined
pprCPInfo (Constant_Class _) = undefined
pprCPInfo (Constant_String _) = undefined
pprCPInfo (Constant_Fieldref _) = undefined
pprCPInfo (Constant_Methodref _) = undefined
pprCPInfo (Constant_InterfaceMethodref _) = undefined
pprCPInfo (Constant_NameAndType _) = undefined
pprCPInfo (Constant_MethodHandle _) = undefined
pprCPInfo (Constant_MethodType _) = undefined
pprCPInfo (Constant_Dynamic _) = undefined
pprCPInfo (Constant_InvokeDynamic _) = undefined
pprCPInfo (Constant_Module _) = undefined
pprCPInfo (Constant_Package _) = undefined
