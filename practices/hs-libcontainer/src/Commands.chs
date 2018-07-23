{-# LANGUAGE ForeignFunctionInterface #-}

module Commands(
  -- * BaseCommand contains data shared among all commands
  BaseCommand,
  -- * RunnableCommand contains data used for create, run and restore.
  RunnableCommand,
  -- * CreateCommand contains data used for creation of containers
  CreateCommand
  ) where

#include "command.h"

import Foreign.Storable (Storable(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CInt, CBool)
import Foreign.Ptr (Ptr)


data BaseCommand = BaseCommand {
  statePath :: CString,
  criu :: CString,
  systemdCgroup :: Bool,
  rootless :: CInt -- 0: auto, 1: true, 2: false
}

-- Ensure that the get hooks treat *BaseCommand as Ptr BaseCommand
-- rather than Ptr ().
{# pointer *BaseCommand as BaseCommandPtr -> BaseCommand #}

instance Storable BaseCommand where
  sizeOf _ = {# sizeof BaseCommand #}
  alignment _ = {# alignof BaseCommand #}
  peek ptr = do
    statePath <- {# get BaseCommand->statePath #} ptr
    criu <- {# get BaseCommand->criu #} ptr
    systemdCgroup <- {# get BaseCommand->systemdCgroup #} ptr
    rootless <- {# get BaseCommand->rootless #} ptr
    return $ BaseCommand statePath criu systemdCgroup rootless
  poke ptr (BaseCommand statePath criu systemdCgroup rootless) = do
    {# set BaseCommand.statePath #} ptr statePath
    {# set BaseCommand.criu #} ptr criu
    {# set BaseCommand.systemdCgroup #} ptr systemdCgroup
    {# set BaseCommand.rootless #} ptr rootless


data RunnableCommand = RunnableCommand {
  base :: BaseCommandPtr,
  containerID :: CString,
  noPivot :: Bool,
  noNewKeyring :: Bool,
  notifySocket :: CString,
  listenFds :: CInt
}

-- Ensure that the get hooks treat *RunnableCommand as Ptr RunnableCommand
-- rather than Ptr ().
{# pointer *RunnableCommand as RunnableCommandPtr -> RunnableCommand #}

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
    return (RunnableCommand base
                            containerID
                            noPivot
                            noNewKeyring
                            notifySocket
                            listenFds)
  poke ptr (RunnableCommand base containerID noPivot noNewKeyring notifySocket listenFds) = do
    {# set RunnableCommand.base #} ptr base
    {# set RunnableCommand.id #} ptr containerID
    {# set RunnableCommand.noPivot #} ptr noPivot
    {# set RunnableCommand.noNewKeyring #} ptr noNewKeyring
    {# set RunnableCommand.notifySocket #} ptr notifySocket
    {# set RunnableCommand.listenFds #} ptr listenFds


data CreateCommand = CreateCommand {
  runnable :: RunnableCommandPtr,
  bundle :: CString,
  consoleSocket :: CString,
  pidFile :: CString,
  preserveFds :: CInt
}

instance Storable CreateCommand where
  sizeOf _ = {# sizeof CreateCommand #}
  alignment _ = {# alignof CreateCommand #}
  peek ptr = do
    runnable <- {# get CreateCommand->runnable #} ptr
    bundle <- {# get CreateCommand->bundle #} ptr
    consoleSocket <- {# get CreateCommand->consoleSocket #} ptr
    pidFile <- {# get CreateCommand->pidFile #} ptr
    preserveFds <- {# get CreateCommand->preserveFds #} ptr
    return $ CreateCommand runnable bundle consoleSocket pidFile preserveFds
  poke ptr (CreateCommand runnable bundle consoleSocket pidFile preserveFds) = do
    {# set CreateCommand.runnable #} ptr runnable
    {# set CreateCommand.bundle #} ptr bundle
    {# set CreateCommand.consoleSocket #} ptr consoleSocket
    {# set CreateCommand.pidFile #} ptr pidFile
    {# set CreateCommand.preserveFds #} ptr preserveFds
