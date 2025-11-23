{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1

import Data.Char (isAlpha, isDigit)
import Data.List (isPrefixOf, isInfixOf)

import Control.Applicative (Alternative(..))
import Control.Concurrent.STM.TVar(TVar, readTVar, writeTVar, readTVarIO)
import Control.Concurrent.STM(atomically)
import Control.Concurrent (Chan, readChan, writeChan, newChan)
import Control.Exception (catch)

import Prelude hiding (readFile, writeFile)
import System.IO.Strict(readFile)
import System.IO(writeFile)

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \input -> case p input of
        Left err -> Left err
        Right (v, rest) -> Right (f v, rest)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> Left "No parse"

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser p1 <|> Parser p2 = Parser $ \input -> case p1 input of
        Right r -> Right r
        Left _  -> p2 input

instance Applicative Parser where
    pure :: a -> Parser a
    pure v = Parser $ \input -> Right (v, input)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser pf <*> Parser pa = Parser $ \input -> case pf input of
        Left err -> Left err
        Right (f, rest1) -> case pa rest1 of
            Left err -> Left err
            Right (a, rest2) -> Right (f a, rest2)

-- | Parses user's input.
-- Yes, this is pretty much the same parser as in Lib2
-- but with a bit different requirements:
-- 1) It must implement Functor, Applicative and Alternative
-- 2) It must NOT implement Monad, no do-notations
-- 3) pmap with andN become <$> <*>
-- 4) orElse becomes <|>
-- 5) many and many1 become many and some
-- Yes, it will be mostly a copy-paste but an easy one
-- if Lib2 was implemented correctly.

-- | Parse a literal string
parseLiteral :: String -> Parser String
parseLiteral pref = Parser $ \input ->
  if pref `isPrefixOf` input
    then Right (pref, drop (length pref) input)
    else Left ("Expected literal: " ++ pref)

-- | Parse a specific character
parseChar :: Char -> Parser Char
parseChar c = c <$ parseLiteral [c]

-- | Parse a letter (a-z, A-Z)
parseLetter :: Parser Char
parseLetter = Parser $ \case
  [] -> Left "A letter is expected but got empty input."
  (h:t) ->
    if isAlpha h
      then Right (h, t)
      else Left ("A letter is expected, but got " ++ [h] ++ ".")

-- | Parse a word (sequence of letters)
parseWord :: Parser String
parseWord = some parseLetter

-- | Parse a space followed by a word
parseSpaceWithWord :: Parser String
parseSpaceWithWord = (\_ word -> ' ' : word) <$> parseChar ' ' <*> parseWord

-- | Parse a sentence (a word followed by zero or more space+word)
parseSentence :: Parser String
parseSentence = (\first rest -> first ++ concat rest) <$> parseWord <*> many parseSpaceWithWord

-- <dishName> ::= "\"" <string> "\""
-- | Parse a dish name enclosed in double quotes
parseDishName :: Parser String
parseDishName = (\_ name _ -> name) <$> parseChar '"' <*> parseSentence <*> parseChar '"'

-- | Parse a non-zero digit (1-9)
parseNonZeroDigit :: Parser Char
parseNonZeroDigit = Parser $ \case
  (c : cs) | c >= '1' && c <= '9' -> Right (c, cs)
  _ -> Left "Expected non-zero digit"


-- | Parse a sequence of digits (0-9)
parseDigits :: Parser String
parseDigits = Parser $ \input -> let (ds, rest) = span isDigit input in Right (ds, rest)

-- | Parse an integer (non-zero digit followed by digits)
parseInteger :: Parser Integer
parseInteger =
  (\d ds -> read (d:ds))
    <$> parseNonZeroDigit
    <*> parseDigits

-- <dumpable> ::= "Examples"
-- | Parse Dumpable
-- Currently only "Examples" is supported
parseDumpable :: Parser Lib1.Dumpable
parseDumpable = Lib1.Examples <$ parseLiteral "Examples"

-- <dish> ::= "Dish " <dishName> " " <calories>
--          | "BigDish (" <dish> ") (" <dish> ")"
-- | Parse Dish (simple or big)
parseDish :: Parser Lib1.Dish
parseDish = parseSimple <|> parseBig
  where
    parseSimple :: Parser Lib1.Dish
    parseSimple = (\_ name _ cal -> Lib1.Dish name cal) <$> parseLiteral "Dish " <*> parseDishName <*> parseChar ' ' <*> parseInteger

    parseBig :: Parser Lib1.Dish
    parseBig = (\_ d1 _ d2 _ -> Lib1.BigDish d1 d2) <$> parseLiteral "BigDish (" <*> parseDish <*> parseLiteral ") (" <*> parseDish <*> parseLiteral ")"

-- <command> ::= "Dump " <dumpable>
--             | "CreateDish (" <dish> ")"
--             | "RemoveDish " <dishName>
--             | "EatDish (" <dish> ")"
-- | Parse Command (Dump, CreateDish, RemoveDish, EatDish)
parseCommand :: Parser Lib1.Command
parseCommand = pDump <|> pCreate <|> pRemove <|> pEat
  where
    pDump :: Parser Lib1.Command
    pDump = (\_ dumpable -> Lib1.Dump dumpable) <$> parseLiteral "Dump " <*> parseDumpable

    pCreate :: Parser Lib1.Command
    pCreate = (\_ dish _ -> Lib1.CreateDish dish) <$> parseLiteral "CreateDish (" <*> parseDish <*> parseLiteral ")"

    pRemove :: Parser Lib1.Command
    pRemove = (\_ name -> Lib1.RemoveDish name) <$> parseLiteral "RemoveDish " <*> parseDishName

    pEat :: Parser Lib1.Command
    pEat = (\_ dish _ -> Lib1.EatDish dish) <$> parseLiteral "EatDish (" <*> parseDish <*> parseLiteral ")"


ppDumpable :: Lib1.Dumpable -> String
ppDumpable Lib1.Examples = "Examples"

ppName :: String -> String
ppName s = "\"" ++ s ++ "\""

ppDish :: Lib1.Dish -> String
ppDish (Lib1.Dish name cal) =
  "Dish " ++ ppName name ++ " " ++ show cal
ppDish (Lib1.BigDish d1 d2) =
  "BigDish (" ++ ppDish d1 ++ ") (" ++ ppDish d2 ++ ")"

ppCommand :: Lib1.Command -> String
ppCommand (Lib1.Dump dumpable)      = "Dump " ++ ppDumpable dumpable
ppCommand (Lib1.CreateDish dish)    = "CreateDish (" ++ ppDish dish ++ ")"
ppCommand (Lib1.RemoveDish dishName)= "RemoveDish " ++ ppName dishName
ppCommand (Lib1.EatDish dish)       = "EatDish (" ++ ppDish dish ++ ")"

-- | You can change the type to whatever needed. If your domain
-- does not have any state you have to make it up.
-- State:
--   * knownDishes: final menu of base dishes by name with calories
--   * eatenHistory: list of dishes eaten so far
data State = State {
  knownDishes :: [(String, Integer)],
  eatenDishes :: [Lib1.Dish]
} deriving Show

emptyState :: State
emptyState = State [] []

-- Fix this accordingly
executeCommand :: State -> Lib1.Command -> (State, [String])
executeCommand state (Lib1.Dump Lib1.Examples) =
  (state, output)
  where
    kd = knownDishes state
    ed = eatenDishes state
    output =
      "Known dishes:" :
      (if null kd then ["<none>"] else map (\(n,c) -> "  - " ++ n ++ " (" ++ show c ++ " cal)") kd) ++
      ["", "Eaten dishes:"] ++
      (if null ed then ["<none>"] else map (\d -> "  - " ++ ppDish d) ed)

executeCommand state (Lib1.CreateDish dish) =
  let baseDishes = flattenDish dish
      (menu', messages) = foldl addOrUpdate (knownDishes state, []) baseDishes
  in (state { knownDishes = menu' }, reverse messages)
  where
    addOrUpdate :: ([(String, Integer)], [String]) -> (String, Integer) -> ([(String, Integer)], [String])
    addOrUpdate (menu, msgs) (name, cal) =
      if name `elem` map fst menu
      then let menu' = map (\(n,c) -> if n == name then (n,cal) else (n,c)) menu
        in (menu', ("Updated dish: " ++ name ++ " -> " ++ show cal ++ " cal") : msgs)
      else let menu' = menu ++ [(name, cal)]
        in (menu', ("Created dish: " ++ name ++ " (" ++ show cal ++ " cal)") : msgs)

executeCommand state (Lib1.RemoveDish name) =
  let menu = knownDishes state
  in if name `elem` map fst menu
    then (state { knownDishes = filter ((/= name) . fst) menu }, ["Removed dish: " ++ name])
    else (state, ["Dish \"" ++ name ++ "\" not found"])

executeCommand state (Lib1.EatDish dish) =
  case validateDish (knownDishes state) dish of
    Just missing -> (state, ["Dish \"" ++ missing ++ "\" not defined. Create it first with CreateDish."])
    Nothing ->
      let state' = state { eatenDishes = eatenDishes state ++ [dish] }
          msg = "Ate: " ++ ppCommand (Lib1.EatDish dish)
      in (state', [msg])

-- Helper: flatten a Dish into its base (name, calories) list
flattenDish :: Lib1.Dish -> [(String, Integer)]
flattenDish (Lib1.Dish name cal)   = [(name, cal)]
flattenDish (Lib1.BigDish d1 d2)   = flattenDish d1 ++ flattenDish d2

validateDish :: [(String, Integer)] -> Lib1.Dish -> Maybe String
validateDish dishes (Lib1.Dish name cal) =
  if any (\(n, c) -> n == name && c == cal) dishes
    then Nothing
    else Just $ "Dish " ++ name ++ " with " ++ show cal ++ " calories is not in known dishes."
validateDish dishes (Lib1.BigDish d1 d2) =
  case validateDish dishes d1 of
    Just err -> Just err
    Nothing  -> validateDish dishes d2


-- | Business/domain logic happens here.
-- This function makes your program actually usefull.
-- You may print if you want to print, you
-- may mutate state if needed but there must be
-- SINGLE atomically call in the function
-- You do not want to write/read files here.
execute :: TVar State -> Lib1.Command -> IO ()
execute tvar cmd = do
  outputs <- atomically $ do
    state <- readTVar tvar
    let (state', output) = executeCommand state cmd
    writeTVar tvar state'
    return output
  mapM_ putStrLn outputs

data StorageOp = Save String (Chan (Either String ())) | Load (Chan (Either String String))
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request. It must run forever.
-- Modify as needed.
-- You might want to use readFile from `strict` library
-- if you get "resource locked" exceptions under Windows.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  op <- readChan chan
  case op of
    Save content reply -> do
      res <- tryWriteFile "state.txt" content
      writeChan reply res
      storageOpLoop chan

    Load reply -> do
      res <- tryReadFile "state.txt"
      writeChan reply res
      storageOpLoop chan
  where
    tryWriteFile :: String -> String -> IO (Either String ())
    tryWriteFile path content =
      catch (writeFile path content >> pure (Right ()))
            (\e -> pure $ Left ("Write error: " ++ show (e :: IOError)))

    tryReadFile :: String -> IO (Either String String)
    tryReadFile path =
      catch (Right <$> readFile path)
            (\e -> pure $ Left ("Read error: " ++ show (e :: IOError)))

stateToCommands :: State -> [Lib1.Command]
stateToCommands state =
  let createCommands = map (\(name, cal) -> Lib1.CreateDish (Lib1.Dish name cal)) (knownDishes state)
      eatCommands = map Lib1.EatDish (eatenDishes state)
  in createCommands ++ eatCommands

parseCommands :: String -> Either String [Lib1.Command]
parseCommands content =
  mapM parseLine nonEmptyLines
  where
    nonEmptyLines = filter (not . null) (lines content)

    parseLine :: String -> Either String Lib1.Command
    parseLine line =
      case runParser parseCommand line of
        Left err ->
          Left $ "Error parsing line '" ++ line ++ "': " ++ err
        Right (cmd, rest) ->
          if all (`elem` [' ', '\t']) rest
            then Right cmd
            else Left $ "Unexpected characters after command: " ++ rest

-- | This function will be called periodically
-- and on programs' exit. File writes must be performed
-- through `Chan StorageOp`.
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save chan tvar = do
    state <- readTVarIO tvar
    let commands = stateToCommands state
    let content = unlines (map ppCommand commands)

    responseChan <- newChan
    writeChan chan (Save content responseChan)
    readChan responseChan

-- | This function will be called on program start
-- File reads must be performed through `Chan StorageOp`
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load chan tvar = do
  reply <- newChan
  writeChan chan (Load reply)
  res <- readChan reply
  case res of
    Left err ->
      if "does not exist" `isInfixOf` err || "No such file" `isInfixOf` err
        then pure (Right ())
        else pure (Left err)

    Right content ->
      if all null (lines content)
        then pure (Right ())
        else case parseCommands content of
               Left perr -> pure (Left ("PARSE ERROR: " ++ perr))
               Right cmds -> do
                 atomically $ do
                   writeTVar tvar emptyState
                   st0 <- readTVar tvar
                   let stFinal = foldl (\s c -> fst (executeCommand s c)) st0 cmds
                   writeTVar tvar stFinal
                 pure (Right ())
