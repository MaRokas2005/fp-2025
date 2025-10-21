module Lessons.Lesson06 where

import Control.Concurrent (newChan, threadDelay, forkIO,readChan, writeChan, Chan)
import System.Random
import Control.Concurrent.STM
    ( TVar, writeTVar, newTVarIO, modifyTVar )
import Control.Monad.STM
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Control.Concurrent.Async
import Control.Monad (when)


main :: IO ()
main = putStrLn "Hello World"

main' :: IO String
main' = getLine

m :: IO ()
m = do
    line <- getLine
    putStrLn line

queryName :: IO String
queryName = do
    putStrLn "What is your name"
    getLine

aGame :: IO ()
aGame = do
    name <- queryName
    let salute = "Hello, " ++ name
    putStrLn salute

pureF :: String -> IO String
pureF a = do 
    n <- queryName
    return $ n ++ a

pureF' :: IO String
pureF' = return "labas"

-- does not compile
--p :: ()
--p = putStrLn "labas"

action :: IO ()
action = do
    threadDelay 10000000
    putStrLn "Hi"

threading :: IO ()
threading = do
    forkIO action
    forkIO action
    putStrLn "!"

actionC :: Chan String -> IO ()
actionC ch = do
    threadDelay 10000000
    writeChan ch "Hi"

threading' :: IO ()
threading' = do
    ch <- newChan
    forkIO $ actionC ch
    forkIO $ actionC ch
    v1 <- readChan ch
    v2 <- readChan ch
    putStrLn $ v1 ++ ", " ++ v2

actionA :: String -> IO String
actionA v = do
    threadDelay 5000000
    return v

threading'' :: IO String
threading'' = do
    a1 <- async $ actionA "First"
    a2 <- async $ actionA "Second"
    v1 <- wait a1
    v2 <- wait a2
    return $ v1 ++ " " ++ v2

transfer :: TVar Integer -> TVar Integer -> Integer -> STM () 
transfer accA accB amount = do
    a <- readTVar accA
    b <- readTVar accB
    writeTVar accA (a - amount)
    writeTVar accB (b + amount)
    newA <- readTVar accA
    when (newA < 0) retry

runTx :: IO ()
runTx = do
    a <- newTVarIO 50
    b <- newTVarIO 100
    z <- async $ atomically $ transfer a b 51
    atomically $ modifyTVar a (+10)
    _ <- wait z
    ar <- readTVarIO a
    br <- readTVarIO b
    putStrLn $ "a=" ++ show ar ++ ", b=" ++  show br
