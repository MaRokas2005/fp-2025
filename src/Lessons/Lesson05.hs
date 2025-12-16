{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
-- | Notes taken by Andrius Gasiukevičius
module Lessons.Lesson05 where

import Data.Foldable
import Data.Monoid
import Data.Char (isAlpha)
import Data.List (isPrefixOf)

-- | Recall that a monoid is a set equipped with a binary operation satisfying the closure, associativity, and identity element properties.
-- If you're familiar with Groups, you can think of it as a group which does not necessarily have the inverse element property.
-- Lists are an example of monoids.

-- | The 'mappend' function represents the binary operation of a monoid ("monoid" + "append" -> "mappend").
-- Note that many instances of Monoid don't actually 'append' things in the usual sense,
-- so it's better to generally think of 'mappend' as an abstract binary operation.
-- For two lists, 'mappend' represents concatenating them into one list (similarly to the ++ operator).
--
-- >>> mappend [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

-- | 'mempty' returns the identity value of a given monoid ("monoid" + "empty" -> "mempty").
-- 'mempty' does not take in any parameters as input, making it a polymorphic constant (determined by its output type) rather than a "proper" function.
-- The identity value of type '[Integer]' (list containing integers) is an empty list since `l ++ [] == l == [] ++ l` for any list l.
--
-- >>> mempty :: [Integer]
-- []

-- | 'map' takes in a function and a list as input, applies the function to every element in the list and returns the function outputs as a new list.
-- 'Sum' is defined like this (in Data.Monoid):
-- `newtype Sum a = Sum { getSum :: a }`
-- which is basically a wrapper with one type parameter 'a' (It also has some derived instances like Eq, Ord, Show, and Read).
-- So here, we basically convert a list of integers into a list of integer monoids equipped with the addition operation.
--
-- >>> map Sum [1,2,3]
-- [Sum {getSum = 1},Sum {getSum = 2},Sum {getSum = 3}]

-- | Recall that 'getSum $ fold $ map Sum [1,2,3]' is equivalent to 'getSum (fold (map Sum [1,2,3]))'.
-- We already know what 'map Sum [1,2,3]' does from the above explanation.
-- 'fold' can be used on structures containing monoids to fold them using the monoid's associated binary operation,
-- with the identity element of the monoid as the initial value of the accumulator.
-- After folding the list, we get a new monoid 'Sum {getSum = 6}' and unwrap it to extract the value of 'getSum'.
--
-- >>> getSum $ fold $ map Sum [1,2,3]
-- 6

-- | 'Product' is defined in a very similar way to 'Sum'; it should be pretty easy to understand what the following code does
-- using similar reasoning as in the previous example.
--
-- >>> getProduct $ fold $ map Product [1,2,3]
-- 6

-- | Here we define a custom newtype which is similar to Sum, but is restricted to the integers.
newtype MySum = MySum {getSum :: Integer}

-- | Any and All are also instances of Monoid. We can define their newtype using a wrapper, similarly to how Sum and Product were defined.
-- Any is equipped with the binary operation || (logical OR) and has 'False' as its identity value since 'False || True == True' and 'False || False == False'.
-- All is equipped with the binary operation && (logical AND) and has 'True' as its identity value since 'True && True == True' and 'True && False == False'.
-- Below are some examples of folding lists containing Any and All monoids.
--
-- >>> fold $ map All [True, True]
-- All {getAll = True}
--
-- >>> fold $ map All [True, True, False]
-- All {getAll = False}
--
-- >>> fold $ map Any [True, True, False]
-- Any {getAny = True}
--
-- >>> fold $ map Any [False, False]
-- Any {getAny = False}

-- | We define a parser similarly to the way it was done in Lesson 4.
-- This time, the parser can return a value of type 'e' instead of just ErrorMsg as its Left value.
-- Also, ErrorMsg is a list of 'String's instead of a single String.
type ErrorMsg = [String]
type Parser e a = String -> Either e (a, String)

-- | This parser attempts to parse a single letter from the beginning of the string.
-- It works similarly to 'parseLetter' from Lesson 4 (essentially rewriting it to support the new Parser and ErrorMsg definitions).
parseLetter :: Parser ErrorMsg Char
parseLetter [] = Left ["A letter is expected but got empty input"]
parseLetter (h:t) =
  if isAlpha h
    then Right (h, t)
    else Left ["A letter is expected, but got " ++ [h]]

-- >>> parseString ""
-- Left ["At least one value required"]
-- >>> parseString "afds"
-- Right ("afds","")
-- >>> parseString "afds5345"
-- Right ("afds","5345")
-- >>> parseString "afds 5345"
-- Right ("afds"," 5345")
-- | 'parseString' attempts to parse a string. It repeatedly attempts to read letters from the start of the input and
-- succeeds if it is able to find at least one letter. The parser stops upon encountering a non-letter character.
-- (The setup is again similar to the parser from Lesson 4, but with support to the new ErrorMsg and Parser definitions).
parseString :: Parser ErrorMsg String
parseString = many1 parseLetter

-- | The 'many' parser runs another parser `many'` repeatedly on the input
-- (similarly to 'many' from Lesson 4, but this time 'Parser' has an additional type parameter).
-- Note that `many' p' (acc ++ [v]) r` is equivalent to `(many' p' (acc ++ [v])) r`
-- since many' returns a function which takes 'r' as input.
many :: Parser e a -> Parser e [a]
many p = many' p []
  where
    many' p' acc = \input ->
      case p' input of
        Left _ -> Right (acc, input)
        Right (v, r) -> many' p' (acc ++ [v]) r

-- | The parser 'many1' requires at least one value to be parsed.
-- Note that 'many p input' is equivalent to '(many p) input' since 'many p' returns a parser.
many1 :: Parser ErrorMsg a -> Parser ErrorMsg [a]
many1 p = \input ->
  case many p input of
    Left e -> Left e
    Right ([], _) -> Left ["At least one value required"]
    Right a -> Right a

-- | 'pmap' maps a parser which parses values of type 'a' to a parser which parses values of type 'b' using a given function 'f'.
-- The function does not map Left values (error messages); only the parsed values of Right are affected.
-- If the parser p parses a Right value 'Right (v, r)', it gets mapped to 'Right (f v, r)'.
pmap :: (a -> b) -> Parser e a -> Parser e b
pmap f p = \input ->
  case p input of
    Left e -> Left e
    Right (v, r) -> Right (f v, r)

-- | Here we define an algebraic data type 'Food'. Clearly, the most important foods are Pizza and Sushi; the rest can be described by a Custom String.
data Food = Pizza | Sushi | Custom String deriving Show

-- | A semigroup is a set equipped with a binary operation satisfying the closure and associativity properties.
-- You can think of it as a Monoid that does not necessarily have an identity element.
-- | orElse is a function combining two parsers into one.
-- Given two parsers 'p1' and 'p2':
-- 'orElse' returns 'Right r1', if 'p1' parses the input given to the combined parser as a 'Right' type with value 'r1'
-- Otherwise, 'orElse' returns 'Right r2' if 'p2' parses the input given to the combined parser as a 'Right' type with value 'r2'
-- Otherwise, 'orElse' returns 'Left $ e1 <> e2' where '<>' is an alias for 'mappend'.
-- So basically, 'orElse' takes the output of the first parser which parses the input, or returns an error if no parser can process the input.
orElse :: Semigroup e => Parser e a -> Parser e a -> Parser e a
orElse p1 p2 = \input ->
    case p1 input of
        Right r1 -> Right r1
        Left e1 ->
            case p2 input of
                Right r2 -> Right r2
                Left e2 -> Left $ e1 <> e2

-- | and3 is a function combining three parsers into one.
-- It attempts to parse the input using three parsers in a row, with each parser receiving the remaining unparsed text as input.
-- If any parser returns an error, the combined parser returns an error as well.
-- If all three parsers successfully parse the input, a tuple of parsed values '(v1, v2, v3)' is returned as a Right value.
and3 :: Parser e a -> Parser e b -> Parser e c -> Parser e (a, b, c)
and3 p1 p2 p3 input =
    case p1 input of
        Left e1 -> Left e1
        Right (v1, r1) ->
            case p2 r1 of
                Left e2 -> Left e2
                Right (v2, r2) ->
                    case p3 r2 of
                        Left e3 -> Left e3
                        Right (v3, r3) -> Right ((v1, v2, v3), r3)
-- | Expanding the definition of 'Parser', we see that keyword :: String -> String -> Either ErrorMsg (String, String)
-- Hence 'keyword' can be understood as a function which takes in two strings and returns an Either (by function associativity).
-- It checks if a given prefix is a prefix of the input being parsed and returns a Right value with the prefix and remaining input if this is the case.
-- Otherwise, it returns a Left value (a list containing an error message).
keyword :: String -> Parser ErrorMsg String
keyword prefix input =
    if prefix `isPrefixOf` input then
        Right (prefix, drop (length prefix) input)
    else Left [prefix ++ " is expected, got " ++ input]

-- | 'ws' parses whitespace (tab or space) characters (at least one) using 'keyword' and 'orElse'.
ws :: Parser ErrorMsg [String]
ws = many1 (keyword " " `orElse` keyword "\t")

-- | 'const x y' always evaluates to 'x'.
-- | 'parsePizza' returns a parser which returns a Right value whenever the input text starts with "pizza".
-- Due to the parser map applied on the 'keyword "pizza"' function, the value of Right gets replaced by
-- the actual Pizza (of type Food) (so "pizza" becomes Pizza).
parsePizza :: Parser ErrorMsg Food
parsePizza = pmap (const Pizza) $ keyword "pizza"

-- | parseSushi works very similarly to 'parsePizza', but for sushi instead.
parseSushi :: Parser ErrorMsg Food
parseSushi = pmap (const Sushi) $ keyword "sushi"

-- | parseCustom parses a string of the form "custom <whitespace> <string>" using a combined 'and3' parser.
-- This output of this parser gets mapped to a Custom food output using 'pmap'.
parseCustom :: Parser ErrorMsg Food
parseCustom = pmap (\(_, _, s) -> Custom s) $ and3 (keyword "custom") ws parseString

-- | parseFood parses a food item using the `orElse` function to combine multiple parsers into one.
-- Some examples:
--
-- >>> parseFood "pizza fdsf"
-- Right (Pizza," fdsf")
-- >>> parseFood "sushi"
-- Right (Sushi,"")
-- >>> parseFood "custom buritto "
-- Right (Custom "buritto"," ")
-- >>> parseFood "customburitto "
-- Left ["pizza is expected, got customburitto ","sushi is expected, got customburitto ","At least on value required"]
-- >>> parseFood "custom   "
-- Left ["pizza is expected, got custom   ","sushi is expected, got custom   ","At least on value required"]
parseFood :: Parser ErrorMsg Food
-- orElse (orElse parsePizza parseSushi) parseCustom
-- The above is an alternative way to write 
parseFood = parsePizza `orElse` parseSushi `orElse` parseCustom
