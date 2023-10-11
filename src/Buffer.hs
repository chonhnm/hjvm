module Buffer where

import ClassFile
import ClassFileChecker (checkAttrLength)
import ClassFileParser (ClassFileParser (getMajorVersion))
import Control.Monad (liftM2)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks, local, withReaderT)
import Data.Binary (Get, getWord8)
import Data.Binary.Get (getByteString, getDoublebe, getFloatbe, getInt32be, getInt64be, getWord16be, getWord32be, isEmpty, runGet)
import Data.ByteString.Lazy as BL hiding (elem)
import Data.Char (chr)
import Data.Text as T hiding (elem)
import GHC.Base (assert)
import Numeric (showHex)
import Util

parseMagic :: Get U4
parseMagic = do
  w <- getWord32be
  if w /= 0xcafebabe
    then fail $ "magic code check failed: 0x" ++ showHex (toInteger w) ""
    else return w

parseMinorVersion :: Get U2
parseMinorVersion = getWord16be

parseMajorVersion :: Get U2
parseMajorVersion = do
  major <- getWord16be
  if major < java_min_support_version
    then error $ "Unknown major version: " ++ show major
    else
      if major > jvm_classfile_major_version
        then error $ "UnSupported major version: " ++ show major
        else return major

parseVersion :: Get Version
parseVersion = do
  minor <- parseMinorVersion
  major <- parseMajorVersion
  case javaVersion major of
    Just version -> return (minor, major, version)
    Nothing -> fail $ "Unknow major version: " ++ show (toInteger major)

parseConstantPoolCount :: Get ConstantPoolCount
parseConstantPoolCount = getWord16be

-- Start parseConstantPoolInfo
parseConstantPoolInfo :: MajorVersionReader CPInfo
parseConstantPoolInfo = do
  tag <- lift getWord8
  case tag of
    1 -> lift parseConstantUtf8
    3 -> lift parseConstantInteger
    4 -> lift parseConstantFloat
    5 -> lift parseConstantLong
    6 -> lift parseConstantDouble
    7 -> lift parseConstantClass
    8 -> lift parseConstantString
    9 -> lift parseConstantFieldref
    10 -> lift parseConstantMethodref
    11 -> lift parseConstantInterfaceMethodref
    12 -> lift parseConstantNameAndType
    15 -> parseConstantMethodHandle
    16 -> parseConstantMethodType
    17 -> parseConstantDynamic
    18 -> parseConstantInvokeDynamic
    19 -> parseConstantModule
    20 -> parseConstantPackage
    _ -> fail $ "unknow constant pool tag: " ++ show (toInteger tag)

parseConstantUtf8 :: Get CPInfo
parseConstantUtf8 = do
  len <- getWord16be
  str <- getByteString $ fromIntegral len
  return $ Constant_Utf8 $ decodeUtf8Jvm str

parseConstantInteger :: Get CPInfo
parseConstantInteger = Constant_Integer <$> getInt32be

parseConstantFloat :: Get CPInfo
parseConstantFloat = Constant_Float <$> getFloatbe

-- TODO: Long and Double take up two entries in the constant_pool table
parseConstantLong :: Get CPInfo
parseConstantLong = Constant_Long <$> getInt64be

parseConstantDouble :: Get CPInfo
parseConstantDouble = Constant_Double <$> getDoublebe

parseConstantClass :: Get CPInfo
parseConstantClass = Constant_Class <$> getWord16be

parseConstantString :: Get CPInfo
parseConstantString = Constant_Class <$> getWord16be

parseConstantFieldref :: Get CPInfo
parseConstantFieldref = liftM2 Constant_Fieldref getWord16be getWord16be

parseConstantMethodref :: Get CPInfo
parseConstantMethodref = liftM2 Constant_Methodref getWord16be getWord16be

parseConstantInterfaceMethodref :: Get CPInfo
parseConstantInterfaceMethodref = liftM2 Constant_InterfaceMethodref getWord16be getWord16be

parseConstantNameAndType :: Get CPInfo
parseConstantNameAndType = liftM2 Constant_NameAndType getWord16be getWord16be

parseConstantMethodHandle :: MajorVersionReader CPInfo
parseConstantMethodHandle = do
  major <- ask
  kind <- assert (major >= java_7_version) lift getWord8
  Constant_MethodHandle (toEnum $ fromIntegral kind) <$> lift getWord16be

parseConstantMethodType :: MajorVersionReader CPInfo
parseConstantMethodType = do
  major <- ask
  assert (major >= java_7_version) Constant_MethodType <$> lift getWord16be

parseConstantDynamic :: MajorVersionReader CPInfo
parseConstantDynamic = do
  major <- ask
  assert (major >= java_11_version) liftM2 Constant_Dynamic (lift getWord16be) (lift getWord16be)

parseConstantInvokeDynamic :: MajorVersionReader CPInfo
parseConstantInvokeDynamic = do
  major <- ask
  assert (major >= java_7_version) liftM2 Constant_InvokeDynamic (lift getWord16be) (lift getWord16be)

parseConstantModule :: MajorVersionReader CPInfo
parseConstantModule = do
  major <- ask
  assert (major >= java_9_version) Constant_Module <$> lift getWord16be

parseConstantPackage :: MajorVersionReader CPInfo
parseConstantPackage = do
  major <- ask
  assert (major >= java_9_version) Constant_Module <$> lift getWord16be

-- End parseConstantPoolInfo
parseAccessFlag :: Get U2
parseAccessFlag = getWord16be

parseThisClass :: Get U2
parseThisClass = getWord16be

parseSuperClass :: Get U2
parseSuperClass = getWord16be

parseInterfacesCount :: Get U2
parseInterfacesCount = getWord16be

parseInterface :: Get U2
parseInterface = getWord16be

parseFieldsCount :: Get U2
parseFieldsCount = getWord16be

parseField :: ClassFileReader FieldInfo
parseField = do
  flag <- lift getWord16be
  nameIndex <- lift getWord16be
  descIndex <- lift getWord16be
  attrCount <- lift getWord16be
  attrs <- parseList attrCount parseAttributeInfo
  return $ FieldInfo flag nameIndex descIndex attrs

parseList :: (Monad m, Integral n) => n -> m a -> m [a]
parseList 0 _ = return []
parseList n m | n > 0 = do
  val <- m
  vals <- parseList (n - 1) m
  return $ val : vals
parseList _ _ = error "n is less than zero."

parseMethodsCount :: Get U2
parseMethodsCount = getWord16be

parseMethod :: ClassFileReader MethodInfo
parseMethod = do
  flags <- lift getWord16be
  ni <- lift getWord16be
  di <- lift getWord16be
  cout <- lift getWord16be
  attrs <- parseList cout parseAttributeInfo
  return $ MethodInfo flags ni di attrs

parseAttributesCount :: Get U2
parseAttributesCount = getWord16be

parseConstantValue :: ClassFileReader AttrInfo
parseConstantValue = do
  idx <- lift getWord16be
  return $ ConstantValue idx

parseCode :: ClassFileReader AttrInfo
parseCode = do
  maxStack <- lift getWord16be
  maxLocals <- lift getWord16be
  codeLength <- lift getWord32be
  code <- lift $ parseList codeLength getWord8
  etl <- lift getWord16be
  et <- parseList etl parseExceptionTable
  ac <- lift getWord16be
  attrs <- parseList ac parseAttributeInfo
  return $
    Code
      CodeAttr
        { ca_max_stack = maxStack,
          ca_max_locals = maxLocals,
          ca_code = code,
          ca_exception_table = et,
          ca_attributes = attrs
        }

parseExceptionTable :: ClassFileReader ExceptionTable
parseExceptionTable = do
  startPc <- lift getWord16be
  endPc <- lift getWord16be
  handlerPc <- lift getWord16be
  ExceptionTable startPc endPc handlerPc <$> lift getWord16be

parseStackMapTable :: ClassFileReader AttrInfo
parseStackMapTable = do
  numberOfEntries <- lift getWord16be
  entries <- parseList numberOfEntries parseStackMapFrame
  return $ StackMapTable entries

parseStackMapFrame :: ClassFileReader StackMapFrame
parseStackMapFrame = do
  tag <- lift getWord8
  case tag of
    x | x `elem` [0 .. 63] -> return $ Same_Frame tag
    x | x `elem` [64 .. 127] -> do
      stack <- parseVerificationTypeInfo
      return $ Same_locals_1_stack_item_frame tag [stack]
    247 -> do
      offset <- lift getWord16be
      stack <- parseVerificationTypeInfo
      return $ Same_locals_1_stack_item_frame_extented tag offset [stack]
    x | x `elem` [248 .. 250] -> Chop_frame tag <$> lift getWord16be
    251 -> Same_frame_extented tag <$> lift getWord16be
    x | x `elem` [252 .. 254] -> do
      offset <- lift getWord16be
      locals <- parseList (tag - 251) parseVerificationTypeInfo
      return $ Append_frame tag offset locals
    255 -> do
      offset <- lift getWord16be
      numberOfLocals <- lift getWord16be
      locals <- parseList numberOfLocals parseVerificationTypeInfo
      nosi <- lift getWord16be
      stack <- parseList nosi parseVerificationTypeInfo
      return $ Full_frame tag offset numberOfLocals locals nosi stack
    _ -> error $ "Unknown StackMapFrame frame_type: " ++ show tag

parseVerificationTypeInfo :: ClassFileReader VerificationTypeInfo
parseVerificationTypeInfo = do
  tag <- lift getWord8
  case tag of
    0 -> return Top_variable_info
    1 -> return Integer_variable_info
    2 -> return Float_variable_info
    3 -> return Double_variable_info
    4 -> return Long_variable_info
    5 -> return Null_variable_info
    6 -> return UninitializedThis_variable_info
    7 -> Object_variable_info <$> lift getWord16be
    8 -> Uninitialized_variable_info <$> lift getWord16be
    _ -> error $ "Unknown verificationTypeInfo: " ++ show tag

parseExceptions :: ClassFileReader AttrInfo
parseExceptions = do
  noe <- lift getWord16be
  eit <- parseList noe $ lift getWord16be
  return $ Exceptions eit

parseInnerClasses :: ClassFileReader AttrInfo
parseInnerClasses = do
  noc <- lift getWord16be
  classes <- parseList noc parseInnerClass
  return $ InnerClasses classes

parseInnerClass :: ClassFileReader InnerClass
parseInnerClass = do
  icii <- lift getWord16be
  ocii <- lift getWord16be
  ini <- lift getWord16be
  icaf <- lift getWord16be
  return $ InnerClass icii ocii ini icaf

parseEnclosingMethod :: ClassFileReader AttrInfo
parseEnclosingMethod = do
  ci <- lift getWord16be
  mi <- lift getWord16be
  return $ EnclosingMethod ci mi

parseSynthetic :: ClassFileReader AttrInfo
parseSynthetic = return Synthetic

parseSignature :: ClassFileReader AttrInfo
parseSignature = do
  si <- lift getWord16be
  return $ Signature si

parseSourceFile :: ClassFileReader AttrInfo
parseSourceFile = do
  si <- lift getWord16be
  return $ SourceFile si

parseSourceDebugExtension :: U4 -> ClassFileReader AttrInfo
parseSourceDebugExtension len = do
  de <- parseList len $ lift getWord8
  return $ SourceDebugExtension de

parseLineNumberTalbe :: ClassFileReader AttrInfo
parseLineNumberTalbe = do
  lntl <- lift getWord16be
  lnt <-
    parseList
      lntl
      ( do
          startPc <- lift getWord16be
          lineNumber <- lift getWord16be
          return $ LineNumber startPc lineNumber
      )
  return $ LineNumberTable lnt

parseLocalVariableTable :: ClassFileReader AttrInfo
parseLocalVariableTable = do
  lvtl <- lift getWord16be
  lvt <- parseList lvtl parseLocalVariable
  return $ LocalVariableTable lvt

parseLocalVariable :: ClassFileReader LocalVariable
parseLocalVariable = do
  startPc <- lift getWord16be
  len <- lift getWord16be
  ni <- lift getWord16be
  di <- lift getWord16be
  idx <- lift getWord16be
  return $ LocalVariable startPc len ni di idx

parseLocalVariableTypeTable :: ClassFileReader AttrInfo
parseLocalVariableTypeTable = do
  lvttl <- lift getWord16be
  lvtt <- parseList lvttl parseLocalVariableType
  return $ LocalVariableTypeTable lvtt

parseLocalVariableType :: ClassFileReader LocalVariableType
parseLocalVariableType = do
  sp <- lift getWord16be
  len <- lift getWord16be
  ni <- lift getWord16be
  si <- lift getWord16be
  idx <- lift getWord16be
  return $ LocalVariableType sp len ni si idx

parseDeprecated :: ClassFileReader AttrInfo
parseDeprecated = return Deprecated

parseRuntimeVisibleAnnotations :: ClassFileReader AttrInfo
parseRuntimeVisibleAnnotations = do
  na <- lift getWord16be
  annos <- parseList na parseAnnotation
  return $ RuntimeInvisibleAnnotations annos

parseAnnotation :: ClassFileReader Annotation
parseAnnotation = do
  ti <- lift getWord16be
  nevp <- lift getWord16be
  evp <-
    parseList
      nevp
      ( do
          eni <- lift getWord16be
          val <- parseElementValue
          return (eni, val)
      )
  return $ Annotation ti evp

parseElementValue :: ClassFileReader ElementValue
parseElementValue = do
  tag <- lift getWord8
  let ch = chr $ fromIntegral tag
  case ch of
    'B' -> EV_byte <$> lift getWord16be
    'C' -> EV_char <$> lift getWord16be
    'D' -> EV_double <$> lift getWord16be
    'F' -> EV_float <$> lift getWord16be
    'I' -> EV_int <$> lift getWord16be
    'J' -> EV_long <$> lift getWord16be
    'S' -> EV_short <$> lift getWord16be
    'Z' -> EV_boolean <$> lift getWord16be
    's' -> EV_string <$> lift getWord16be
    'e' -> liftM2 EV_enum (lift getWord16be) (lift getWord16be)
    'c' -> EV_class <$> lift getWord16be
    '@' -> EV_anno <$> parseAnnotation
    '[' -> do
      nv <- lift getWord16be
      vals <- parseList nv parseElementValue
      return $ EV_array vals
    _ -> error $ "Unknown element value tag: " ++ show ch

parseRuntimeInvisibleAnnotations :: ClassFileReader AttrInfo
parseRuntimeInvisibleAnnotations = do
  na <- lift getWord16be
  annos <- parseList na parseAnnotation
  return $ RuntimeInvisibleAnnotations annos

parseRuntimeVisibleParameterAnnotations :: ClassFileReader AttrInfo
parseRuntimeVisibleParameterAnnotations = do
  np <- lift getWord8
  pa <-
    parseList
      np
      ( do
          na <- lift getWord16be
          parseList na parseAnnotation
      )
  return $ RuntimeVisibleParameterAnnotations pa

parseRuntimeInvisibleParameterAnnotations :: ClassFileReader AttrInfo
parseRuntimeInvisibleParameterAnnotations = do
  np <- lift getWord8
  pa <-
    parseList
      np
      ( do
          na <- lift getWord16be
          parseList na parseAnnotation
      )
  return $ RuntimeInvisibleParameterAnnotations pa

parseRuntimeVisibleTypeAnnotations :: ClassFileReader AttrInfo
parseRuntimeVisibleTypeAnnotations = do
  na <- lift getWord16be
  annos <- parseList na parseTypeAnnotation
  return $ RuntimeVisibleTypeAnnotations annos

parseTypeAnnotation :: ClassFileReader TypeAnnotation
parseTypeAnnotation = do
  targetType <- lift getWord8
  targetInfo <- case targetType of
    x | x `elem` [0x00, 0x01] -> Type_parameter_target <$> lift getWord8
    0x10 -> Supertype_target <$> lift getWord16be
    x | x `elem` [0x11, 0x12] -> liftM2 Type_parameter_bound_target (lift getWord8) (lift getWord8)
    x | x `elem` [0x13 .. 0x15] -> return Empty_target
    0x16 -> Formal_parameter_target <$> lift getWord8
    0x17 -> Throws_target <$> lift getWord16be
    x | x `elem` [0x40, 0x41] -> do
      tl <- lift getWord16be
      tbl <-
        parseList
          tl
          ( do
              sp <- lift getWord16be
              len <- lift getWord16be
              idx <- lift getWord16be
              return (sp, len, idx)
          )
      return $ Localvar_target tbl
    0x42 -> Catch_target <$> lift getWord16be
    x | x `elem` [0x43 .. 0x46] -> Offset_target <$> lift getWord16be
    x | x `elem` [0x47 .. 0x4B] -> liftM2 Type_argument_target (lift getWord16be) (lift getWord8)
    _ -> error $ "Unknown target_type: " ++ show targetType
  targetPath <- do
    pl <- lift getWord8
    parseList
      pl
      ( do
          tpk <- lift getWord8
          tai <- lift getWord8
          return (tpk, tai)
      )
  typeIndex <- lift getWord16be
  nevp <- lift getWord16be
  evp <-
    parseList
      nevp
      ( do
          eni <- lift getWord16be
          val <- parseElementValue
          return (eni, val)
      )
  return $ TypeAnnotation targetType targetInfo targetPath typeIndex evp

parseRuntimeInvisibleTypeAnnotations :: ClassFileReader AttrInfo
parseRuntimeInvisibleTypeAnnotations = do
  na <- lift getWord16be
  annos <- parseList na parseTypeAnnotation
  return $ RuntimeInvisibleTypeAnnotations annos

parseAnnotationDefault :: ClassFileReader AttrInfo
parseAnnotationDefault = AnnotationDefault <$> parseElementValue

parseBootstrapMethods :: ClassFileReader AttrInfo
parseBootstrapMethods = do
  nbm <- lift getWord16be
  bm <- parseList nbm parseBootstrapMethod
  return $ BootstrapMethods bm

parseBootstrapMethod :: ClassFileReader BootstrapMethod
parseBootstrapMethod = do
  bmr <- lift getWord16be
  nba <- lift getWord16be
  ba <- parseList nba (lift getWord16be)
  return $ BootstrapMethod bmr ba

parseMethodParameters :: ClassFileReader AttrInfo
parseMethodParameters = do
  pc <- lift getWord8
  ps <-
    parseList
      pc
      ( do
          ni <- lift getWord16be
          af <- lift getWord16be
          return (ni, af)
      )
  return $ MethodParameters ps

parseModule :: ClassFileReader AttrInfo
parseModule = do
  moduleNameIndex <- lift getWord16be
  moduleFlags <- lift getWord16be
  moduleVersionIndex <- lift getWord16be
  requiresCount <- lift getWord16be
  reqs <-
    parseList
      requiresCount
      ( do
          ri <- lift getWord16be
          rf <- lift getWord16be
          rvi <- lift getWord16be
          return (ri, rf, rvi)
      )
  exportsCount <- lift getWord16be
  eps <-
    parseList
      exportsCount
      ( do
          ei <- lift getWord16be
          ef <- lift getWord16be
          etc <- lift getWord16be
          eti <- parseList etc (lift getWord16be)
          return (ei, ef, eti)
      )
  opensCount <- lift getWord16be
  ops <-
    parseList
      opensCount
      ( do
          opi <- lift getWord16be
          opf <- lift getWord16be
          otc <- lift getWord16be
          oti <- parseList otc (lift getWord16be)
          return (opi, opf, oti)
      )
  usesCount <- lift getWord16be
  usesIndex <- parseList usesCount (lift getWord16be)
  providesCount <- lift getWord16be
  prd <-
    parseList
      providesCount
      ( do
          pri <- lift getWord16be
          pwc <- lift getWord16be
          pwi <- parseList pwc (lift getWord16be)
          return (pri, pwi)
      )
  return $
    Module
      ModuleAttr
        { uses_index = usesIndex,
          requires = reqs,
          provides = prd,
          opens = ops,
          module_version_index = moduleVersionIndex,
          module_name_index = moduleNameIndex,
          module_flags = moduleFlags,
          exports = eps
        }

parseModulePackages :: ClassFileReader AttrInfo
parseModulePackages = do
  pc <- lift getWord16be
  pis <- parseList pc (lift getWord16be)
  return $ ModulePackages pis

parseModuleMainClass :: ClassFileReader AttrInfo
parseModuleMainClass = do
  mci <- lift getWord16be
  return $ ModuleMainClass mci

parseNestHost :: ClassFileReader AttrInfo
parseNestHost = do
  hci <- lift getWord16be
  return $ NestHost hci

parseNestMembers :: ClassFileReader AttrInfo
parseNestMembers = do
  noc <- lift getWord16be
  cls <- parseList noc (lift getWord16be)
  return $ NestMembers cls

parseRecord :: ClassFileReader AttrInfo
parseRecord = do
  cc <- lift getWord16be
  comps <- parseList cc parseRecordComponentInfo
  return $ Record comps

parseRecordComponentInfo :: ClassFileReader RecordComponentInfo
parseRecordComponentInfo = do
  nameIndex <- lift getWord16be
  desIndex <- lift getWord16be
  attrCount <- lift getWord16be
  attrs <- parseList attrCount parseAttributeInfo
  return $ RecordComponentInfo nameIndex desIndex attrs

parsePermittedSubclasses :: ClassFileReader AttrInfo
parsePermittedSubclasses = do
  noc <- lift getWord16be
  cls <- parseList noc (lift getWord16be)
  return $ PermittedSubclasses cls

parseAttributeInfo :: ClassFileReader AttributeInfo
parseAttributeInfo = do
  cp <- asks constantPool
  attrNameIdx <- lift getWord16be
  len <- lift getWord32be
  let maybeAttrTag = cpUtf8 cp attrNameIdx
  case maybeAttrTag of
    Left err -> error $ show err
    Right attrTag -> do
      let str = T.unpack attrTag
      attr <- case str of
        "ConstantValue" -> parseConstantValue
        "Code" -> parseCode
        "StackMapTable" -> parseStackMapTable
        "Exceptions" -> parseExceptions
        "InnerClasses" -> parseInnerClasses
        "EnclosingMethod" -> parseEnclosingMethod
        "Synthetic" -> parseSynthetic
        "Signature" -> parseSignature
        "SourceFile" -> parseSourceFile
        "SourceDebugExtension" -> parseSourceDebugExtension len
        "LineNumberTable" -> parseLineNumberTalbe
        "LocalVariableTable" -> parseLocalVariableTable
        "LocalVariableTypeTable" -> parseLocalVariableTypeTable
        "Deprecated" -> parseDeprecated
        "RuntimeVisibleAnnotations" -> parseRuntimeVisibleAnnotations
        "RuntimeInvisibleAnnotations" -> parseRuntimeInvisibleAnnotations
        "RuntimeVisibleParameterAnnotations" -> parseRuntimeVisibleParameterAnnotations
        "RuntimeInvisibleParameterAnnotations" -> parseRuntimeInvisibleParameterAnnotations
        "RuntimeVisibleTypeAnnotations" -> parseRuntimeVisibleTypeAnnotations
        "RuntimeInvisibleTypeAnnotations" -> parseRuntimeInvisibleTypeAnnotations
        "AnnotationDefault" -> parseAnnotationDefault
        "BootstrapMethods" -> parseBootstrapMethods
        "MethodParameters" -> parseMethodParameters
        "Module" -> parseModule
        "ModulePackages" -> parseModulePackages
        "ModuleMainClass" -> parseModuleMainClass
        "NestHost" -> parseNestHost
        "NestMembers" -> parseNestMembers
        "Record" -> parseRecord
        "PermittedSubclasses" -> parsePermittedSubclasses
        _ -> error $ "Unspported attribute: " ++ str
      return $ AttributeInfo len attr

type ClassFileReader = ReaderT ClassFile Get

type MajorVersion = U2

type MajorVersionReader = ReaderT MajorVersion Get

parseClassFile :: ClassFileReader ClassFile
parseClassFile = do
  _ <- lift parseMagic
  minorVer <- lift parseMinorVersion
  majorVer <- lift parseMajorVersion
  cpc <- lift parseConstantPoolCount
  cp <- parseList (cpc - 1) $ withReaderT getMajorVersion parseConstantPoolInfo
  accFlags <- lift parseAccessFlag
  thisClazz <- lift parseThisClass
  superClazz <- lift parseSuperClass
  interfaceCount <- lift parseInterfacesCount
  ifs <- parseList interfaceCount $ lift parseInterface
  fieldsCount <- lift parseFieldsCount
  etpClazz <- ask
  let initClass =
        etpClazz
          { minorVersion = minorVer,
            majorVersion = majorVer,
            constantPool = cp,
            accessFlags = accFlags,
            thisClass = thisClazz,
            superClass = superClazz,
            interfaces = ifs
          }
  fds <- parseList fieldsCount (local (const initClass) parseField)
  methodsCount <- lift parseMethodsCount
  mds <- parseList methodsCount (local (const initClass {fields = fds}) parseMethod)
  attrCount <- lift parseAttributesCount
  attrs <- parseList attrCount (local (const initClass {fields = fds, methods = mds}) parseAttributeInfo)
  ept <- lift isEmpty
  assert ept return $
    ClassFile
      { minorVersion = minorVer,
        majorVersion = majorVer,
        constantPool = cp,
        accessFlags = accFlags,
        thisClass = thisClazz,
        superClass = superClazz,
        interfaces = ifs,
        fields = fds,
        methods = mds,
        attributes = attrs
      }

runParseClassFile :: FilePath -> IO ()
runParseClassFile file = do
  input <- BL.readFile file
  let cf = loadClassFile input
  case cf of
    Left err -> print $ err ++ ".."
    Right _ -> print "classfile: ok!"

type Err = String

loadClassFile :: ByteString -> Either Err ClassFile
loadClassFile file =
  let cf = runGet (runReaderT parseClassFile emptyClassFile) file
      err = checkAttrLength cf
   in case err of
        Nothing -> Right cf
        Just str -> Left str