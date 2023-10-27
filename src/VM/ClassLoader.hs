module VM.ClassLoader where

import ClassFileConsts (U4)
import Data.Map qualified as Map
import Data.Text qualified as T
import VM.Class (Class (..))

data ClassLoader = ClassLoader
  { cld_id :: U4,
    cld_classes :: Map.Map T.Text Class
  }

class IClassLoader cld where
  registerClass :: cld -> Class -> cld
  findClassByName :: cld -> T.Text -> Maybe Class

instance IClassLoader ClassLoader where
  registerClass cld cls =
    let oldMap = cld_classes cld
        name = cl_name cls
        newMap = Map.insert name cls oldMap
     in cld {cld_classes = newMap}
  findClassByName cld name = Map.lookup name (cld_classes cld)