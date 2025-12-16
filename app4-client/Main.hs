{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
    ( parseRequest,
      getResponseBody,
      httpLBS,
      setRequestBodyLBS,
      setRequestHeader,
      setRequestMethod )
import System.Environment (lookupEnv)

import qualified Lib1
import qualified Lib3 as S
import qualified Lib4

program :: Lib4.Program [String]
program = do
  a <- Lib4.dump Lib1.Examples
  b <- Lib4.createDish (Lib1.Dish "Soup" 100)
  c <- Lib4.eatDish (Lib1.Dish "Soup" 100)
  d <- Lib4.dump Lib1.Examples
  pure (a ++ [""] ++ b ++ [""] ++ c ++ [""] ++ d)

sendHttp :: String -> Lib1.Command -> IO [String]
sendHttp url cmd = do
  req0 <- parseRequest url
  let body = LBS8.pack (Lib4.toCliCommand cmd)
      req =
        setRequestMethod "POST"
          $ setRequestHeader "Content-Type" ["text/plain; charset=utf-8"]
          $ setRequestBodyLBS body req0
  resp <- httpLBS req
  pure (lines (LBS8.unpack (getResponseBody resp)))

main :: IO ()
main = do
  mode <- fmap (fromMaybe "http") (lookupEnv "FP2025_MODE")
  url <- fmap (fromMaybe "http://localhost:3000/command") (lookupEnv "FP2025_SERVER_URL")

  out <-
    if mode == "local"
      then do
        let (res, _st) = Lib4.runLocalWith S.emptyState program
        pure res
      else Lib4.runHttp (sendHttp url) program

  mapM_ putStrLn out