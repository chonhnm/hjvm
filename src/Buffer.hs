module Buffer where

import ClassFile
import Control.Monad (liftM2)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks, local)
import Data.Binary (Get, getWord8)
import Data.Binary.Get (getByteString, getDoublebe, getFloatbe, getInt32be, getInt64be, getWord16be, getWord32be)
import Data.Text as T
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
parseMajorVersion = getWord16be

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
parseConstantPoolInfo :: Get ConstantPoolInfo
parseConstantPoolInfo = do
  tag <- getWord8
  cpInfo <- case tag of
    1 -> parseConstantUtf8
    3 -> parseConstantInteger
    4 -> parseConstantFloat
    5 -> parseConstantLong
    6 -> parseConstantDouble
    7 -> parseConstantClass
    8 -> parseConstantString
    9 -> parseConstantFieldref
    10 -> parseConstantMethodref
    11 -> parseConstantInterfaceMethodref
    12 -> parseConstantNameAndType
    15 -> parseConstantMethodHandle
    16 -> parseConstantMethodType
    17 -> parseConstantDynamic
    18 -> parseConstantInvokeDynamic
    19 -> parseConstantModule
    20 -> parseConstantPackage
    _ -> fail $ "unknow constant pool tag: " ++ show (toInteger tag)
  return $ ConstantPoolInfo tag cpInfo

parseConstantUtf8 :: Get CPInfo
parseConstantUtf8 = do
  len <- getWord16be
  str <- getByteString $ fromIntegral len
  return $ (Constant_Utf8 . ConstantUtf8) (decodeUtf8Jvm str)

parseConstantInteger :: Get CPInfo
parseConstantInteger = Constant_Integer . ConstantInteger <$> getInt32be

parseConstantFloat :: Get CPInfo
parseConstantFloat = Constant_Float . ConstantFloat <$> getFloatbe

-- TODO: Long and Double take up two entries in the constant_pool table
parseConstantLong :: Get CPInfo
parseConstantLong = Constant_Long . ConstantLong <$> getInt64be

parseConstantDouble :: Get CPInfo
parseConstantDouble = Constant_Double . ConstantDouble <$> getDoublebe

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

parseConstantMethodHandle :: Get CPInfo
parseConstantMethodHandle = do
  kind <- getWord8
  Constant_MethodHandle (toEnum $ fromIntegral kind) <$> getWord16be

parseConstantMethodType :: Get CPInfo
parseConstantMethodType = Constant_MethodType <$> getWord16be

parseConstantDynamic :: Get CPInfo
parseConstantDynamic = liftM2 Constant_Dynamic getWord16be getWord16be

parseConstantInvokeDynamic :: Get CPInfo
parseConstantInvokeDynamic = liftM2 Constant_InvokeDynamic getWord16be getWord16be

parseConstantModule :: Get CPInfo
parseConstantModule = Constant_Module <$> getWord16be

parseConstantPackage :: Get CPInfo
parseConstantPackage = Constant_Module <$> getWord16be

-- End parseConstantPoolInfo
parseAccessFlag :: Get ClassAccessFlag
parseAccessFlag = toEnum . fromIntegral <$> getWord16be

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

parseField :: Get FieldInfo
parseField = do
  flag <- getWord16be
  nameIndex <- getWord16be
  descIndex <- getWord16be
  attrCount <- getWord16be
  attrs <- parseList attrCount parseAttribute
  return $ FieldInfo flag nameIndex descIndex attrs

parseList :: Monad m => U2 -> m a -> m [a]
parseList 0 _ = return []
parseList n m = do
  val <- m
  vals <- parseList (n - 1) m
  return $ val : vals

parseMethodsCount :: Get U2
parseMethodsCount = getWord16be

parseMethod :: Get MethodInfo
parseMethod = do
  flags <- getWord16be
  ni <- getWord16be
  di <- getWord16be
  cout <- getWord16be
  attrs <- parseList cout parseAttribute
  return $ MethodInfo flags ni di attrs

parseAttributesCount :: Get U2
parseAttributesCount = getWord16be

parseAttribute :: Get AttributeInfo
parseAttribute = undefined

parseClassFile :: Get ClassFile
parseClassFile = undefined

parseAttributeReader :: ClassFileReader AttributeInfo
parseAttributeReader = do
  cp <- asks constantPool
  attrNameIdx <- lift getWord16be
  let ConstantUtf8 attrTag = constUtf8 cp (fromIntegral attrNameIdx)
  let str = T.unpack attrTag
  case str of
    "ConstantValue" -> undefined
    "Code" -> undefined
    "StackMapTable" -> undefined
    "Exceptions" -> undefined
    "InnerClasses" -> undefined
    "EnclosingMethod" -> undefined
    "Synthetic" -> undefined
    "Signature" -> undefined
    "SourceFile" -> undefined
    "SourceDebugExtension" -> undefined
    "LineNumberTable" -> undefined
    "LocalVariableTable" -> undefined
    "LocalVariableTypeTable" -> undefined
    "Deprecated" -> undefined
    "RuntimeVisibleAnnotations" -> undefined
    "RuntimeInvisibleAnnotations" -> undefined
    "RuntimeVisibleParameterAnnotations" -> undefined
    "RuntimeInvisibleParameterAnnotations" -> undefined
    "RuntimeVisibleTypeAnnotations" -> undefined
    "RuntimeInvisibleTypeAnnotations" -> undefined
    "AnnotationDefault" -> undefined
    "BootstrapMethods" -> undefined
    "MethodParameters" -> undefined
    "Module" -> undefined
    "ModulePackages" -> undefined
    "ModuleMainClass" -> undefined
    "NestHost" -> undefined
    "NestMembers" -> undefined
    "Record" -> undefined
    "PermittedSubclasses" -> undefined
    _ -> error $ "Unspported attribute: " ++ str

type ClassFileReader = ReaderT ClassFile Get
