module Env where
import Control.Monad.Trans.Reader (ReaderT)
import Util (MyErr)
import ClassFile (ClassFile)
import ConstantPool (ConstantPoolInfo)
import ClassFileConsts (U2)


type CPReader = ReaderT Env MyErr

data Env = Env
  { envClassFile :: ClassFile,
    envPool :: ConstantPoolInfo,
    envCurPoolIdx :: U2
  }

