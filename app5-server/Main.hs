{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (modifyMVar, newMVar, readMVar)
import Control.Exception (try)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Network.HTTP.Types.Status (status400)
import System.Environment (lookupEnv)
import Web.Scotty (body, get, post, raw, scotty, status)

import qualified Lib3 as S
import qualified Lib4

commandsFile :: FilePath
commandsFile = "lab4-commands.txt"

loadCommands :: IO [String]
loadCommands = do
  e <- try (readFile commandsFile) :: IO (Either IOError String)
  case e of
    Left _ -> pure []
    Right content -> pure (filter (not . null) (lines content))

saveCommands :: [String] -> IO ()
saveCommands cmds = writeFile commandsFile (unlines cmds)

replayCommands :: [String] -> S.State
replayCommands = foldl step S.emptyState
  where
    step st line =
      case Lib4.parseCommandString line of
        Left _ -> st
        Right cmd ->
          let (st', _out) = Lib4.executeCommandPure st cmd
           in st'

main :: IO ()
main = do
  port <- maybe 3000 read <$> lookupEnv "FP2025_PORT"

  cmds0 <- loadCommands
  let st0 = replayCommands cmds0

  -- store both state and command log
  store <- newMVar (st0, cmds0)

  -- periodic save (requirement)
  _ <- forkIO $ forever $ do
    threadDelay (5 * 1000000)
    (_st, cmds) <- readMVar store
    saveCommands cmds

  scotty port $ do
    get "/" $ raw "ok"

    post "/command" $ do
      bs <- body
      let cmdStr = LBS8.unpack bs

      case Lib4.parseCommandString cmdStr of
        Left err -> do
          status status400
          raw (LBS8.pack ("PARSE ERROR: " ++ err))
        Right cmd -> do
          (_st', _cmds', outLines) <-
            liftIO $
              modifyMVar store $ \(st, cmds) -> do
                let (st', out) = Lib4.executeCommandPure st cmd
                let cmds' = cmds ++ [cmdStr]
                saveCommands cmds' -- save after each command
                pure ((st', cmds'), (st', cmds', out))

          raw (LBS8.pack (unlines outLines))