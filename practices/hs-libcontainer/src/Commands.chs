{-# LANGUAGE ForeignFunctionInterface #-}

module Commands where

#include "command.h"

import Foreign.Storable (Storable(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CInt, CBool)
import Foreign.Ptr (Ptr)

type Bool = CInt

data BaseCommand = BaseCommand {
  statePath :: CString,
  criu :: CString,
  systemdCgroup :: CBool,
  rootless :: Ptr CBool
}

instance Storable BaseCommand where
  sizeOf _ = {# sizeof BaseCommand #}
  alignment _ = {# alignof BaseCommand #}
  peek ptr = do
    statePath <- {# get BaseCommand->statePath #} ptr
    criu <- {# get BaseCommand->criu #} ptr
    systemdCgroup <- {# get BaseCommand->systemdCgroup #} ptr
    rootless <- {# get BaseCommand->rootless #} ptr
    return BaseCommand statePath criu systemdCgroup rootless
  poke ptr (BaseCommand statePath criu systemdCgroup rootless) = do
    {# set BaseCommand.statePath #} ptr statePath
    {# set BaseCommand.criu #} ptr criu
    {# set BaseCommand.systemdCgroup #} ptr systemdCgroup
    {# set BaseCommand.rootless #} ptr rootless


data RunnableCommand = RunnableCommand {
  base :: Ptr BaseCommand,
  containerID :: CString,
  noPivot :: CBool,
  noNewKeyring :: CBool,
  notifySocket :: CString,
  listenFds :: CInt
}

instance Storable RunnableCommand where
  sizeOf _ = {# sizeof RunnableCommand #}
  alignment _ = {# alignof RunnableCommand #}
  peek ptr = do
    base <- {# get RunnableCommand->base #} ptr
    containerID <- {# get RunnableCommand->id #} ptr
    noPivot <- {# get RunnableCommand->noPivot #} ptr
    noNewKeyring <- {# get RunnableCommand->noNewKeyring #} ptr
    notifySocket <- {# get RunnableCommand->notifySocket #} ptr
    listenFds <- {# get RunnableCommand->listenFds #} ptr
    return RunnableCommand containerID noPivot noNewKeyring notifySocket listenFds
  poke ptr (RunnableCommand base containerID noPivot noNewKeyring notifySocket listenFds) = do
    {# set RunnableCommand.base #} ptr base
    {# set RunnableCommand.id #} ptr containerID
    {# set RunnableCommand.noPivot #} ptr noPivot
    {# set RunnableCommand.noNewKeyring #} ptr noNewKeyring
    {# set RunnableCommand.notifySocket #} ptr notifySocket
    {# set RunnableCommand.listenFds #} ptr listenFds

data CreateCommand = CreateCommand {
  runnable :: Ptr RunnableCommand,
  bundle :: CString,
  consoleSocket :: CString,
  pidFile :: CString,
  preserveFds :: CInt
}
