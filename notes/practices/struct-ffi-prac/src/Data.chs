{-# LANGUAGE ForeignFunctionInterface #-}

module Data(InputStruct(..), ReturnStruct(..)) where

#include "data.h"

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CInt)

data InputStruct = InputStruct {
  val :: CInt
} deriving (Show)

instance Storable InputStruct where
  sizeOf _ = {#sizeof InputStruct #}
  alignment _ = {#alignof InputStruct #}
  peek ptr = InputStruct <$> ({#get InputStruct->val #} ptr)
  poke ptr (InputStruct val) = {#set InputStruct.val #} ptr val

type ReturnStruct = InputStruct -- For simplicity
