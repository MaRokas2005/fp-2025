{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp) where

import qualified Lib1
import qualified Lib2

import Control.Monad.STM
import Control.Concurrent.STM.TVar(TVar)
import Control.Concurrent.Async
import Control.Concurrent (Chan, readChan, writeChan)

-- You can change the type to whatever needed
newtype State = State ()

-- Fix this accordingly
emptyState :: State
emptyState = State()

-- Business/domain logic happens here.
execute :: TVar State -> Lib1.Command -> IO ()
execute _ _ = error "Implement me 1"

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request. It must run forever.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop c = do
  _ <- readChan c
  return $ error "Implement me 2"

-- This function will be called periodically
-- and on programs' exit. File writes must be performed
-- through `Chan StorageOp`.
save :: Chan StorageOp -> TVar State -> IO ()
save _ _ = putStrLn "Implement me 3"

-- This function will be called on program start
-- File reads must be performed through `Chan StorageOp`
load :: Chan StorageOp -> TVar State -> IO ()
load _ _ = putStrLn "Implement me 4"
