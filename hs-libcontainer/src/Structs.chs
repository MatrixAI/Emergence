{-# LANUGAGE ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#include structs.h

module Structs where

import Foreign.Storable (Storable)
import Foreign.C.Types (CString)
import Foreign.Ptr (Pre, nullPtr)

data linuxIntelRdt = LinuxIntelRdt {
  l3CacheSchema :: CString
}

instance Storable LinuxIntelRdt where
  sizeOf _ = #{sizeof LinuxIntelRdt}
  alignment _ = #{alignment LinuxIntelRdt}
  peek ptr = dos
    lcs <- peekCString $ #{ptr c_type, c_string_field} ptr
    return LinuxIntelRdt lcs
  poke ptr (LinuxIntelRdt lcs) = do
    withCStringLen (take maxLen value)
      $ uncurry (copyArray
      $ #{ptr c_type, c_string_field} ptr)
