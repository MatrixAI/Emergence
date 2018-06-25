{-# LANGUAGE ForeignFunctionInterface #-}

#include "oci-spec.h"

module OCISpec (
  LinuxIntelRdt(..)
) where

import Foreign.Storable (Storable)
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

data LinuxIntelRdt = LinuxIntelRdt {
  l3CacheSchema :: CString
} deriving (Show, Eq)

instance Storable LinuxIntelRdt where
  sizeOf _ = {#sizeof LinuxIntelRdt #}
  alignment _ = {#alignof LinuxIntelRdt #}
  -- peek :: Ptr LinuxIntelRdt -> IO (LinuxIntelRdt)
  peek ptr = LinuxIntelRdt <$> {#get LinuxIntelRdt->l3CacheSchema #} ptr
  poke ptr (LinuxIntelRdt lcs) = {#set LinuxIntelRdt.l3CacheSchema #} ptr lcs

data Test = Test {
  linuxIntelRdt :: Ptr'LinuxIntelRdt
} deriving (Show, Eq)

{#pointer *LinuxIntelRdt as Ptr'LinuxIntelRdt -> LinuxIntelRdt #}

instance Storable Test where
  sizeOf _ = {#sizeof Test #}
  alignment _ = {#alignof Test #}
  peek ptr = Test <$> (peekByteOff ptr 0 :: IO Ptr'LinuxIntelRdt)
  poke ptr (Test lir) = {#set Test.linuxIntelRdt #} ptr lir
