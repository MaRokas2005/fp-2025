{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib4
  (
    ErrorMsg,
    Input,
    Parser,
    Program,
    parseCommand,
    parseCommandString,
    executeCommandPure,
    toCliCommand,
    dump,
    createDish,
    removeDish,
    eatDish,
    runHttp,
    runLocal,
    runLocalWith,
  )
where

import Control.Applicative (Alternative (..), many, some)
import Control.Monad.Free (Free (..), liftF)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import qualified Control.Monad.Trans.State.Strict as ST
import Data.Char (isAlpha, isDigit)
import Data.List (isPrefixOf)
import qualified Lib1
import qualified Lib2
import qualified Lib3 as S
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    chooseInt,
    elements,
    frequency,
    oneof,
    sized,
  )
import Control.Monad (replicateM)

type ErrorMsg = String
type Input = String

type Parser a = ExceptT ErrorMsg (State Input) a

throwP :: ErrorMsg -> Parser a
throwP = throwE

getInput :: Parser Input
getInput = lift get

putInput :: Input -> Parser ()
putInput s = lift (put s)

parseLiteral :: String -> Parser String
parseLiteral pref = do
  input <- getInput
  if pref `isPrefixOf` input
    then putInput (drop (length pref) input) >> pure pref
    else throwP ("Expected literal: " ++ show pref)

parseChar :: Char -> Parser Char
parseChar c = do
  _ <- parseLiteral [c]
  pure c

parseLetter :: Parser Char
parseLetter = do
  input <- getInput
  case input of
    [] -> throwP "A letter is expected but got empty input."
    (h : t) ->
      if isAlpha h
        then putInput t >> pure h
        else throwP ("A letter is expected, but got " ++ [h] ++ ".")

parseWord :: Parser String
parseWord = some parseLetter

parseSpaceWithWord :: Parser String
parseSpaceWithWord = (\_ w -> ' ' : w) <$> parseChar ' ' <*> parseWord

parseSentence :: Parser String
parseSentence = (\first rest -> first ++ concat rest) <$> parseWord <*> many parseSpaceWithWord

-- BNF: name ::= "\"" sentence "\""
parseDishName :: Parser String
parseDishName = (\_ name _ -> name) <$> parseChar '"' <*> parseSentence <*> parseChar '"'

parseNonZeroDigit :: Parser Char
parseNonZeroDigit = do
  input <- getInput
  case input of
    (c : cs) | c >= '1' && c <= '9' -> putInput cs >> pure c
    _ -> throwP "Expected non-zero digit"

parseDigits :: Parser String
parseDigits = do
  input <- getInput
  let (ds, rest) = span isDigit input
  putInput rest
  pure ds

parseInteger :: Parser Integer
parseInteger = (\d ds -> read (d : ds)) <$> parseNonZeroDigit <*> parseDigits

-- BNF: dumpable ::= "Examples"
parseDumpable :: Parser Lib1.Dumpable
parseDumpable = Lib1.Examples <$ parseLiteral "Examples"

-- BNF:
-- dish ::= "Dish " name " " int
--       |  "BigDish (" dish ") (" dish ")"
parseDish :: Parser Lib1.Dish
parseDish = parseSimple <|> parseBig
  where
    parseSimple =
      (\_ name _ cal -> Lib1.Dish name cal)
        <$> parseLiteral "Dish "
        <*> parseDishName
        <*> parseChar ' '
        <*> parseInteger

    parseBig =
      (\_ d1 _ d2 _ -> Lib1.BigDish d1 d2)
        <$> parseLiteral "BigDish ("
        <*> parseDish
        <*> parseLiteral ") ("
        <*> parseDish
        <*> parseLiteral ")"

eof :: Parser ()
eof = do
  rest <- getInput
  if null rest
    then pure ()
    else throwP ("Expected EOF, but got trailing: " ++ take 30 rest)

-- BNF:
-- command ::= "Dump " dumpable
--          |  "CreateDish (" dish ")"
--          |  "RemoveDish " name
--          |  "EatDish (" dish ")"
parseCommand :: Parser Lib1.Command
parseCommand = do
  c <- pDump <|> pCreate <|> pRemove <|> pEat
  eof
  pure c
  where
    pDump =
      (\_ d -> Lib1.Dump d)
        <$> parseLiteral "Dump "
        <*> parseDumpable

    pCreate =
      (\_ d _ -> Lib1.CreateDish d)
        <$> parseLiteral "CreateDish ("
        <*> parseDish
        <*> parseLiteral ")"

    pRemove =
      (\_ name -> Lib1.RemoveDish name)
        <$> parseLiteral "RemoveDish "
        <*> parseDishName

    pEat =
      (\_ d _ -> Lib1.EatDish d)
        <$> parseLiteral "EatDish ("
        <*> parseDish
        <*> parseLiteral ")"

-- Convenience for server/client
parseCommandString :: String -> Either ErrorMsg Lib1.Command
parseCommandString = evalState (runExceptT parseCommand)

-- Use the official formatter from Lib2 (matches tests + labs)
toCliCommand :: Lib1.Command -> String
toCliCommand = Lib2.toCliCommand

-- =========================
-- Arbitrary (QuickCheck)
-- =========================

genWord :: Gen String
genWord = do
  n <- chooseInt (1, 10)
  let letters = ['a' .. 'z'] ++ ['A' .. 'Z']
  replicateM n (elements letters)

genSentence :: Gen String
genSentence = do
  k <- chooseInt (1, 3)
  ws <- replicateM k genWord
  pure (unwords ws)

genCalories :: Gen Integer
genCalories = fromIntegral <$> chooseInt (100, 2000)

genDish :: Gen Lib1.Dish
genDish = sized go
  where
    go 0 = Lib1.Dish <$> genSentence <*> genCalories
    go n =
      frequency
        [ (4, Lib1.Dish <$> genSentence <*> genCalories),
          (1, Lib1.BigDish <$> go (n `div` 2) <*> go (n `div` 2))
        ]

instance Arbitrary Lib1.Command where
  arbitrary :: Gen Lib1.Command
  arbitrary =
    oneof
      [ pure (Lib1.Dump Lib1.Examples),
        Lib1.CreateDish <$> genDish,
        Lib1.RemoveDish <$> genSentence,
        Lib1.EatDish <$> genDish
      ]

-- =========================
-- Shared domain logic (server + local interpreter)
-- =========================

ppName :: String -> String
ppName s = "\"" ++ s ++ "\""

ppDish :: Lib1.Dish -> String
ppDish (Lib1.Dish name cal) = "Dish " ++ ppName name ++ " " ++ show cal
ppDish (Lib1.BigDish d1 d2) = "BigDish (" ++ ppDish d1 ++ ") (" ++ ppDish d2 ++ ")"

flattenDish :: Lib1.Dish -> [(String, Integer)]
flattenDish (Lib1.Dish name cal) = [(name, cal)]
flattenDish (Lib1.BigDish d1 d2) = flattenDish d1 ++ flattenDish d2

validateDish :: [(String, Integer)] -> Lib1.Dish -> Maybe String
validateDish dishes (Lib1.Dish name cal) =
  if any (\(n, c) -> n == name && c == cal) dishes
    then Nothing
    else Just $ "Dish " ++ name ++ " with " ++ show cal ++ " calories is not in known dishes."
validateDish dishes (Lib1.BigDish d1 d2) =
  case validateDish dishes d1 of
    Just err -> Just err
    Nothing -> validateDish dishes d2

executeCommandPure :: S.State -> Lib1.Command -> (S.State, [String])
executeCommandPure st (Lib1.Dump Lib1.Examples) =
  ( st,
    output
  )
  where
    kd = S.knownDishes st
    ed = S.eatenDishes st
    output =
      "Known dishes:"
        : (if null kd then [""] else map (\(n, c) -> " - " ++ n ++ " (" ++ show c ++ " cal)") kd)
        ++ ["", "Eaten dishes:"]
        ++ (if null ed then [""] else map (\d -> " - " ++ ppDish d) ed)
executeCommandPure st (Lib1.CreateDish dish) =
  let baseDishes = flattenDish dish
      (menu', messages) = foldl addOrUpdate (S.knownDishes st, []) baseDishes
   in (st {S.knownDishes = menu'}, reverse messages)
  where
    addOrUpdate :: ([(String, Integer)], [String]) -> (String, Integer) -> ([(String, Integer)], [String])
    addOrUpdate (menu, msgs) (name, cal) =
      if name `elem` map fst menu
        then
          let menu' = map (\(n, c) -> if n == name then (n, cal) else (n, c)) menu
           in (menu', ("Updated dish: " ++ name ++ " -> " ++ show cal ++ " cal") : msgs)
        else
          let menu' = menu ++ [(name, cal)]
           in (menu', ("Created dish: " ++ name ++ " (" ++ show cal ++ " cal)") : msgs)
executeCommandPure st (Lib1.RemoveDish name) =
  let menu = S.knownDishes st
   in if name `elem` map fst menu
        then (st {S.knownDishes = filter ((/= name) . fst) menu}, ["Removed dish: " ++ name])
        else (st, ["Dish \"" ++ name ++ "\" not found"])
executeCommandPure st (Lib1.EatDish dish) =
  case validateDish (S.knownDishes st) dish of
    Just missing ->
      ( st,
        [ "Dish \"" ++ missing ++ "\" not defined.",
          "Create it first with CreateDish."
        ]
      )
    Nothing ->
      let st' = st {S.eatenDishes = S.eatenDishes st ++ [dish]}
       in (st', ["Ate: " ++ Lib2.toCliCommand (Lib1.EatDish dish)])

-- =========================
-- Free Monad DSL (client)
-- =========================

data DslF next
  = DumpF Lib1.Dumpable ([String] -> next)
  | CreateDishF Lib1.Dish ([String] -> next)
  | RemoveDishF String ([String] -> next)
  | EatDishF Lib1.Dish ([String] -> next)

instance Functor DslF where
  fmap f (DumpF d k) = DumpF d (f . k)
  fmap f (CreateDishF d k) = CreateDishF d (f . k)
  fmap f (RemoveDishF n k) = RemoveDishF n (f . k)
  fmap f (EatDishF d k) = EatDishF d (f . k)

type Program a = Free DslF a

dump :: Lib1.Dumpable -> Program [String]
dump d = liftF (DumpF d id)

createDish :: Lib1.Dish -> Program [String]
createDish d = liftF (CreateDishF d id)

removeDish :: String -> Program [String]
removeDish n = liftF (RemoveDishF n id)

eatDish :: Lib1.Dish -> Program [String]
eatDish d = liftF (EatDishF d id)

runHttp :: (Lib1.Command -> IO [String]) -> Program a -> IO a
runHttp send = go
  where
    go (Pure a) = pure a
    go (Free op) =
      case op of
        DumpF d k -> send (Lib1.Dump d) >>= go . k
        CreateDishF d k -> send (Lib1.CreateDish d) >>= go . k
        RemoveDishF n k -> send (Lib1.RemoveDish n) >>= go . k
        EatDishF d k -> send (Lib1.EatDish d) >>= go . k

runLocal :: Program a -> ST.State S.State a
runLocal = go
  where
    go (Pure a) = pure a
    go (Free op) =
      case op of
        DumpF d k -> step (Lib1.Dump d) k
        CreateDishF d k -> step (Lib1.CreateDish d) k
        RemoveDishF n k -> step (Lib1.RemoveDish n) k
        EatDishF d k -> step (Lib1.EatDish d) k

    step :: Lib1.Command -> ([String] -> Program a) -> ST.State S.State a
    step cmd k = do
      st <- ST.get
      let (st', out) = executeCommandPure st cmd
      ST.put st'
      go (k out)

runLocalWith :: S.State -> Program a -> (a, S.State)
runLocalWith st p = ST.runState (runLocal p) st