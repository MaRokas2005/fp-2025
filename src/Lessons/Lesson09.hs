{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lessons.Lesson09 where

import Lessons.Lesson08(Parser(..), threeLetters)

import Control.Applicative

import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

import Control.Monad
import Data.Foldable

-- >>> sequenceA [Just 42, Just 5]
-- Just [42,5]

-- >>> sequenceA [Just 42, Just 5, Nothing]
-- Nothing

-- >>> sequenceA [[42], [13, 45]]
-- [[42,13],[42,45]]

-- >>> sequenceA [[42], [13, 45], []]
-- []

-- >>> sequenceA [Right 43, Right 45]
-- Right [43,45]

-- >>> sequenceA [Left 0, Right 43, Right 45]
-- Left 0

-- >>> sequenceA $ Right (Right 45)
-- Right (Right 45)

-- >>> sequenceA $ Right (Right 45)
-- Right (Right 45)-- Left 45

-- >>> sequenceA $ Just (Right 45)
-- Right (Just 45)

-- >>> :t sequenceA [getLine, getLine, getLine]
-- sequenceA [getLine, getLine, getLine] :: IO [String]

-- >>> :t [getLine, getLine, getLine]
-- [getLine, getLine, getLine] :: [IO String]

queryAge :: String -> IO Integer
queryAge name = do
    putStrLn $ "What is your age, " ++ name ++ "?"
    read <$> getLine

-- >>> :t mapM queryAge ["VI", "A", "U"]
-- mapM queryAge ["VI", "A", "U"] :: IO [Integer]

-- >>> :t map queryAge ["VI", "A", "U"]
-- map queryAge ["VI", "A", "U"] :: [IO Integer]

-- >>> :t mapM_ queryAge ["VI", "A", "U"]
-- mapM_ queryAge ["VI", "A", "U"] :: IO ()

-- >>> :t forM_ ["VI", "A", "U"] queryAge
-- forM_ ["VI", "A", "U"] queryAge :: IO ()

-- >>> liftM2 (+) (Just 4) (Just 6)
-- Just 10

-- >>>  (+) <$> (Just 4) <*> (Just 6)
-- Just 10

-- >>> liftM3 (\a b c -> a + b - c) (Just 4) (Just 6) Nothing
-- Nothing

-- >>> liftA3 (\a b c -> a + b - c) (Just 4) (Just 6) Nothing
-- Nothing

-- >>> :t threeLetters
-- threeLetters :: Parser String

-- >>> :t sequenceA [threeLetters, threeLetters, threeLetters]
-- sequenceA [threeLetters, threeLetters, threeLetters] :: Parser [String]

-- >>> :t runParser (sequenceA [threeLetters, threeLetters, threeLetters])
-- runParser (sequenceA [threeLetters, threeLetters, threeLetters]) :: String -> Either String ([String], String)

-- >>> runParser (sequenceA [threeLetters, threeLetters, threeLetters]) "asdasdasd!"
-- Right (["asd","asd","asd"],"!")

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    ma >>= mf = Parser $ \input ->
        case runParser ma input of
            Left e1 -> Left e1
            Right (a, r1) ->
                case runParser (mf a) r1 of
                    Left e2 -> Left e2
                    Right (b, r2) -> Right (b, r2)

-- >>> runParser threeThreeLetterWords "asdasdasd!"
-- Right (["asd","asd","asd"],"!")
threeThreeLetterWords:: Parser [String]
threeThreeLetterWords = do
    a <- threeLetters
    b <- threeLetters
    c <- threeLetters
    pure [a, b, c]

-- >>> runState stateful "initial"
-- (7,"I am a new state")
stateful :: State String Int
stateful = do
    value <- get
    put "I am a new state"
    pure $ length value

-- >>> runState combined "initial"
-- ((7,16),"I am a new state")
combined :: State String (Int, Int)
combined = do
    a <- stateful
    b <- stateful
    pure (a, b)

-- >>> runState combined' "initial"
-- ((7,16),"I am a new state")
combined' :: State String (Int, Int)
combined' = (,) <$>  stateful <*> stateful

-- >>> runState combined'' "initial"
-- ((7,16),"I am a new state")
combined'' :: State String (Int, Int)
combined'' = liftA2 (,) stateful stateful
