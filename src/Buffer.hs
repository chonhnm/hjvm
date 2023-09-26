module Buffer where

import ClassFile
import Control.Monad (liftM2)
import Data.Binary (Get, getWord8)
import Data.Binary.Get (getByteString, getDoublebe, getFloatbe, getInt32be, getInt64be, getWord16be, getWord32be)
import Numeric (showHex)
import Util

parseMagic :: Get Magic
parseMagic = do
  w <- getWord32be
  if w /= 0xcafebabe
    then fail $ "magic code check failed: 0x" ++ showHex (toInteger w) ""
    else return w

parseMinorVersion :: Get MinorVersion
parseMinorVersion = getWord16be

parseMajorVersion :: Get MajorVersion
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
  return $ Constant_Utf8 (decodeUtf8Jvm str)

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