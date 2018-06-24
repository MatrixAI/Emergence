{-# LANUGAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#include <ocispec.h>

module OCISpec where

import Foreign.Storable (Storable)
import Foreign.C.Types (CString)
import Foreign.Ptr (Pre, nullPtr)

data LinuxIntelRdt = LinuxIntelRdt {
  l3CacheSchema :: CString
} deriving (Show, Eq)

instance Storable LinuxIntelRdt where
  sizeOf _ = #{size LinuxIntelRdt}
  alignment _ = #{alignment LinuxIntelRdt}
  peek ptr = dos
    lcs <- peekCString $ #{ptr c_type, l3CacheSchema} ptr
    return LinuxIntelRdt lcs
  poke ptr (LinuxIntelRdt lcs) = do
    withCStringLen (take maxLen value)
      $ uncurry (copyArray
      $ #{ptr c_type, c_string_field} ptr)
