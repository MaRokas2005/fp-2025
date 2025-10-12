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

-- Combinators
orElse :: Parser a -> Parser a -> Parser a
orElse p q s = case p s of
  Right r -> Right r
  Left _  -> q s

and2 :: Parser a -> Parser b -> Parser (a,b)
and2 p q s = case p s of
  Left e -> Left e
  Right (a,s1) -> case q s1 of
    Left e -> Left e
    Right (b,s2) -> Right ((a,b), s2)

-- >>> parseLiteral "Dump " "Dump Examplesrest"
-- Right ("Dump ","Examplesrest")
parseLiteral :: String -> Parser String
parseLiteral pref s =
  if pref `isPrefixOf` s
  then Right (pref, drop (length pref) s)
  else Left ("Expected " ++ show pref)

parseChar :: Char -> Parser Char
parseChar c s = case s of
  (x:xs) | x == c -> Right (x, xs)
  _ -> Left ("Expected char " ++ show c)

-- >>> parseDishName "\"Chicken Soup\"restbv h\""
-- Right ("Chicken Soup","restbv h\"")
parseString :: Parser String
parseString s =
  let (content, rest) = span (/= '"') s
      valid = case content of
        [] -> False
        _  -> isValid content
  in case rest of
      ('"':xs) | valid -> Right (content, xs)
      _ -> Left "Unterminated or invalid string"
  where
    isValid :: String -> Bool
    isValid xs =
      let parts = words xs
      in not (null parts)
         && all (all isAlpha) parts
         && xs == unwords parts

parseDishName :: Parser String
parseDishName s = case parseChar '"' s of
  Left e -> Left e
  Right (_, s1) -> parseString s1

-- >>> parseNonZeroDigit "8954 \"Chicken Soup\"restbv"
-- Right ('8',"954 \"Chicken Soup\"restbv")
parseNonZeroDigit :: Parser Char
parseNonZeroDigit s = case s of
  (c:cs) | c >= '1' && c <= '9' -> Right (c, cs)
  _ -> Left "Expected non-zero digit"

-- >>> parseInteger "800954 \"Chicken Soup\"restbv"
-- Right (800954," \"Chicken Soup\"restbv")
-- >>> parseInteger "5597fd"
-- Right (5597,"fd")
parseInteger :: Parser Integer
parseInteger s = case s of
  ('0':_) ->
    Left "Leading zeros not allowed"
  _ -> case parseNonZeroDigit s of
        Left e -> Left e
        Right (d, s1) ->
          let (ds, s2) = span isDigit s1
          in Right (read (d:ds), s2)

-- >>> parseDumpable "Examplesrestbv"
-- Right (Examples,"restbv")
parseDumpable :: Parser Lib1.Dumpable
parseDumpable s =
  case parseLiteral "Examples" s of
    Right (_, rest) -> Right (Lib1.Examples, rest)
    Left e -> Left e

-- >>> parseDish "Dish \"Chicken Soup\" 250restbv"
-- >>> parseDish "BigDish (Dish \"Chicken Soup\" 250) (Dish \"Salad\" 150)restbv"
-- Right (Dish "Chicken Soup" 250,"restbv")
-- Right (BigDish (Dish "Chicken Soup" 250) (Dish "Salad" 150),"restbv")
parseDish :: Parser Lib1.Dish
parseDish = parseSimple `orElse` parseBig
  where
    parseSimple :: Parser Lib1.Dish
    parseSimple s =
      case parseLiteral "Dish " s of
        Left e -> Left e
        Right (_, s1) -> case parseDishName s1 of
          Left e -> Left e
          Right (name, s2) -> case parseLiteral " " s2 of
            Left e -> Left e
            Right (_, s3) -> case parseInteger s3 of
              Left e -> Left e
              Right (cal, s4) -> Right (Lib1.Dish name cal, s4)

    parseBig :: Parser Lib1.Dish
    parseBig s =
      case parseLiteral "BigDish (" s of
        Left e -> Left e
        Right (_, s1) -> case parseDish s1 of
          Left e -> Left e
          Right (d1, s2) -> case parseLiteral ") (" s2 of
            Left e -> Left e
            Right (_, s3) -> case parseDish s3 of
              Left e -> Left e
              Right (d2, s4) -> case parseLiteral ")" s4 of
                Left e -> Left e
                Right (_, s5) -> Right (Lib1.BigDish d1 d2, s5)

-- >>> parseCommand "Dump Examplesrestbv"
-- Right (Dump Examples,"restbv")
-- >>> parseCommand "CreateDish (Dish \"Chicken Soup\" 250)"
-- Right (CreateDish (Dish "Chicken Soup" 250),"")
parseCommand :: Parser Lib1.Command
parseCommand = pDump `orElse` pCreate `orElse` pRemove `orElse` pEat
  where
    pDump input =
      case and2 (parseLiteral "Dump ") parseDumpable input of
        Right ((_, d), rest) -> Right (Lib1.Dump d, rest)
        Left e -> Left e

    pCreate input =
      case parseLiteral "CreateDish (" input of
        Left e -> Left e
        Right (_, s1) -> case parseDish s1 of
          Left e -> Left e
          Right (d, s2) -> case parseLiteral ")" s2 of
            Left e -> Left e
            Right (_, rest) -> Right (Lib1.CreateDish d, rest)

    pRemove input =
      case and2 (parseLiteral "RemoveDish ") parseDishName input of
        Right ((_, name), rest) -> Right (Lib1.RemoveDish name, rest)
        Left e -> Left e

    pEat input =
      case parseLiteral "EatDish (" input of
        Left e -> Left e
        Right (_, s1) -> case parseDish s1 of
          Left e -> Left e
          Right (d, s2) -> case parseLiteral ")" s2 of
            Left e -> Left e
            Right (_, rest) -> Right (Lib1.EatDish d, rest)

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
  toCliCommand (Lib1.Dump d)         = "Dump " ++ ppDumpable d
  toCliCommand (Lib1.CreateDish d)   = "CreateDish (" ++ ppDish d ++ ")"
  toCliCommand (Lib1.RemoveDish nm)  = "RemoveDish " ++ ppName nm
  toCliCommand (Lib1.EatDish d)      = "EatDish (" ++ ppDish d ++ ")"

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
