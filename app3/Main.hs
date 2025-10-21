{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib2 qualified
import Lib3 qualified
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )

import Control.Monad.STM
import Control.Concurrent.STM.TVar(TVar)
import Control.Concurrent.Async
import Control.Concurrent.STM (newTVarIO, TVar, atomically, readTVarIO)
import Control.Concurrent (Chan, newChan, writeChan, readChan, threadDelay, forkIO)
import Control.Monad (forever)

type Repl a = HaskelineT IO a

final :: TVar Lib3.State -> Chan Lib3.StorageOp -> Repl ExitDecision
final state chan = do
  liftIO $ Lib3.save chan state >> putStrLn "Goodbye!"
  return Exit

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome! Press [TAB] for auto completion."

completer :: (Monad m) => WordCompleter m
completer _ = return []

cmd :: TVar Lib3.State -> String -> Repl ()
cmd state str = do
  case Lib2.parseCommand str of
    Left e -> liftIO $ putStrLn $ "PARSE ERROR: " ++ e
    Right (c, "") -> liftIO $ Lib3.execute state c
    Right (c, r) -> liftIO $ putStrLn $ "PARSED: " ++ show c ++
        ", but remaining input IS NOT fully consumed: " ++ r

main :: IO ()
main = do
  state <- newTVarIO Lib3.emptyState
  chan <- newChan
  _ <- forkIO $ Lib3.storageOpLoop chan
  Lib3.load chan state
  _ <- forkIO $ forever $ threadDelay 1000000 >> Lib3.save chan state
  evalRepl (const $ pure ">>> ") (cmd state) [] Nothing Nothing (Word completer) ini (final state chan)
