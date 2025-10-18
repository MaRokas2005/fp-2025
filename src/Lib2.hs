{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1
import Data.Char (isAlpha, isDigit)
import Data.List (isPrefixOf)

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

pmap :: (a -> b) -> Parser a -> Parser b
pmap f p input = case p input of
      Left err -> Left err
      Right (v, rest) -> Right (f v, rest)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input = case p1 input of
  Right r -> Right r
  Left _  -> p2 input

and2 :: Parser a -> Parser b -> Parser (a,b)
and2 p1 p2 input = case p1 input of
  Left e -> Left e
  Right (a, rest1) -> case p2 rest1 of
    Left e -> Left e
    Right (b, rest2) -> Right ((a, b), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 p1 p2 p3 input = case and2 p1 p2 input of
    Left e -> Left e
    Right ((v1, v2), rest1) -> case p3 rest1 of
        Left e -> Left e
        Right (v3, rest2) -> Right ((v1, v2, v3), rest2)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
and4 p1 p2 p3 p4 input = case and3 p1 p2 p3 input of
    Left e -> Left e
    Right ((v1, v2, v3), rest1) -> case p4 rest1 of
        Left e -> Left e
        Right (v4, rest2) -> Right ((v1, v2, v3, v4), rest2)

and5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a, b, c, d, e)
and5 p1 p2 p3 p4 p5 input = case and4 p1 p2 p3 p4 input of
    Left e -> Left e
    Right ((v1, v2, v3, v4), rest1) -> case p5 rest1 of
        Left e -> Left e
        Right (v5, rest2) -> Right ((v1, v2, v3, v4, v5), rest2)

-- >>> parseLiteral "Dump " "Dump Examplesrest"
-- Right ("Dump ","Examplesrest")
parseLiteral :: String -> Parser String
parseLiteral pref = pmap (const pref) match
  where
    match :: Parser String
    match s
      | pref `isPrefixOf` s = Right (pref, drop (length pref) s)
      | otherwise = Left ("Expected " ++ show pref)

parseChar :: Char -> Parser Char
parseChar c = pmap (const c) (parseLiteral [c])

-- >>> parseDishName "\"Chicken Soupbh\" restbv h"
-- Right ("Chicken Soupbh"," restbv h")
parseDishName :: Parser String
parseDishName = pmap (\(_, name, _) -> name) (and3 (parseChar '"') inner (parseChar '"'))
  where
    inner :: Parser String
    inner s =
      let (content, rest) = span (/= '"') s
          valid = case content of
                    [] -> False
                    _  -> let parts = words content
                          in not (null parts)
                             && all (all isAlpha) parts
                             && content == unwords parts
      in case rest of
           ('"':_) | valid -> Right (content, rest)
           _               -> Left "Unterminated or invalid string"

-- >>> parseNonZeroDigit "8954 \"Chicken Soup\"restbv"
-- Right ('8',"954 \"Chicken Soup\"restbv")
parseNonZeroDigit :: Parser Char
parseNonZeroDigit s = case s of
  (c:cs) | c >= '1' && c <= '9' -> Right (c, cs)
  _ -> Left "Expected non-zero digit"


-- >>> parseDigits "8954 \"Chicken Soup\"restbv"
-- >>> parseDigits "abc"
-- Right ("8954"," \"Chicken Soup\"restbv")
-- Right ("","abc")
parseDigits :: Parser String
parseDigits s = let (ds, rest) = span isDigit s in Right (ds, rest)

-- >>> parseInteger "800954 \"Chicken Soup\"restbv"
-- Right (800954," \"Chicken Soup\"restbv")
-- >>> parseInteger "5597fd"
-- Right (5597,"fd")
parseInteger :: Parser Integer
parseInteger s = case s of
  ('0':_) -> Left "Leading zeros not allowed"
  _ -> pmap (\(d, ds) -> read (d:ds))
            (and2 parseNonZeroDigit parseDigits) s

-- >>> parseDumpable "Examplesrestbv"
-- Right (Examples,"restbv")
parseDumpable :: Parser Lib1.Dumpable
parseDumpable = pmap (const Lib1.Examples) (parseLiteral "Examples")

-- >>> parseDish "Dish \"Chicken Soup\" 250restbv"
-- >>> parseDish "BigDish (Dish \"Chicken Soup\" 250) (Dish \"Salad\" 150)restbv"
-- Right (Dish "Chicken Soup" 250,"restbv")
-- Right (BigDish (Dish "Chicken Soup" 250) (Dish "Salad" 150),"restbv")
parseDish :: Parser Lib1.Dish
parseDish = parseSimple `orElse` parseBig
  where
    parseSimple :: Parser Lib1.Dish
    parseSimple = pmap (\(_, name, _, cal) -> Lib1.Dish name cal) (and4 (parseLiteral "Dish ") parseDishName (parseChar ' ') parseInteger)

    parseBig :: Parser Lib1.Dish
    parseBig = pmap (\(_, d1, _, d2, _) -> Lib1.BigDish d1 d2) (and5 (parseLiteral "BigDish (") parseDish (parseLiteral ") (") parseDish (parseLiteral ")"))

-- >>> parseCommand "Dump Examplesrestbv"
-- Right (Dump Examples,"restbv")
-- >>> parseCommand "CreateDish (Dish \"Chicken Soup\" 250)"
-- Right (CreateDish (Dish "Chicken Soup" 250),"")
parseCommand :: Parser Lib1.Command
parseCommand = pDump `orElse` pCreate `orElse` pRemove `orElse` pEat
  where
    pDump :: Parser Lib1.Command
    pDump = pmap (\(_, dumpable) -> Lib1.Dump dumpable) (and2 (parseLiteral "Dump ") parseDumpable)

    pCreate :: Parser Lib1.Command
    pCreate = pmap (\(_, dish, _) -> Lib1.CreateDish dish)
              (and3 (parseLiteral "CreateDish (") parseDish (parseLiteral ")"))

    pRemove :: Parser Lib1.Command
    pRemove = pmap (\(_, name) -> Lib1.RemoveDish name)
              (and2 (parseLiteral "RemoveDish ") parseDishName)

    pEat :: Parser Lib1.Command
    pEat = pmap (\(_, dish, _) -> Lib1.EatDish dish)
           (and3 (parseLiteral "EatDish (") parseDish (parseLiteral ")"))

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

ppDumpable :: Lib1.Dumpable -> String
ppDumpable Lib1.Examples = "Examples"

ppDish :: Lib1.Dish -> String
ppDish (Lib1.Dish name cal) = "Dish " ++ ppName name ++ " " ++ show cal
ppDish (Lib1.BigDish d1 d2) =
  "BigDish (" ++ ppDish d1 ++ ") (" ++ ppDish d2 ++ ")"

ppName :: String -> String
ppName s = "\"" ++ s ++ "\""

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.Dump dumpable)         = "Dump " ++ ppDumpable dumpable
  toCliCommand (Lib1.CreateDish dish)   = "CreateDish (" ++ ppDish dish ++ ")"
  toCliCommand (Lib1.RemoveDish dishName)  = "RemoveDish " ++ ppName dishName
  toCliCommand (Lib1.EatDish dish)      = "EatDish (" ++ ppDish dish ++ ")"

instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  Lib1.Dump a        == Lib1.Dump b        = eqDump a b
  Lib1.CreateDish x  == Lib1.CreateDish y  = eqDish x y
  Lib1.RemoveDish x  == Lib1.RemoveDish y  = x == y
  Lib1.EatDish x     == Lib1.EatDish y     = eqDish x y
  _                  == _                  = False

eqDump :: Lib1.Dumpable -> Lib1.Dumpable -> Bool
eqDump Lib1.Examples Lib1.Examples = True

eqDish :: Lib1.Dish -> Lib1.Dish -> Bool
eqDish (Lib1.Dish n1 c1)   (Lib1.Dish n2 c2)   = n1 == n2 && c1 == c2
eqDish (Lib1.BigDish a b)  (Lib1.BigDish x y)  = eqDish a x && eqDish b y
eqDish _                   _                   = False
