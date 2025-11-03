{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib1
    ( examples, Command(..), Dumpable(..), Dish(..)
    ) where

data Dumpable = Examples
  deriving Show

-- Recursive Dish definition
data Dish
    = Dish String Integer
    | BigDish Dish Dish
  deriving Show

-- Root ADT representing grammar commands
data Command
    = Dump Dumpable
    | CreateDish Dish
    | RemoveDish String
    | EatDish Dish
  deriving Show

examples :: [Command]
examples = [
    Dump Examples, 
    CreateDish (Dish "pasta" 200), 
    CreateDish (BigDish (Dish "salad" 100) (Dish "soup" 150)), 
    RemoveDish "pasta", 
    EatDish (Dish "salad" 250), 
    EatDish (BigDish (BigDish (Dish "salad" 100) (Dish "soup" 150)) (Dish "ice cream" 150))
    ]