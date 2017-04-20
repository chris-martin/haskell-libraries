-- Re-assigning variables is a really fun advanced topic!

{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Arrow
import Control.Lens
import Control.Monad.State
import Prelude

-- As we all know, elephants are integers.

type Elephant = Integer

-- First let's define what it means to have an elephant. Lens captures this notion nicely.

class HasElephant a
  where
    elephant :: Lens' a Elephant

-- Now we can define 'function', which assigns a new value to 'elephant'.

function :: (MonadState s m, HasElephant s) => Elephant -> m ()
function x =
    elephant .= x

-- The constraint 'MonadState s m' says our program type 'm' needs to have some state of type 's', and the 'HasElephant s' constraint says that our state 's' must include an elephant.

-- Let's also define a program that prints the elephant.

printElephant :: (MonadIO m, MonadState s m, HasElephant s) => m ()
printElephant = use elephant >>= (print >>> liftIO)

-- This program does I/O (printing), so we have an additional constraint 'MonadIO m' that says our program type 'm' must be able to do I/O.

-- The African forest elephant (Loxodonta cyclotis) is a forest-dwelling species of elephant found in the Congo Basin.

data Congo = Congo
    { _congoElephant :: Elephant
    }
makeLenses ''Congo

-- We must define the way in which the 'Congo' has an elephant.

instance HasElephant Congo
  where
    elephant = congoElephant

-- Now we can write a program. Our program will print the value of 'elephant', then change the value of 'elephant', then print it again.

main' :: StateT Congo IO ()
main' =
  do
    printElephant
    function 2
    printElephant

-- Then we can run this program.

main :: IO ()
main = Congo 0 & runStateT main' & void

-- It prints:
-- 0
-- 2
