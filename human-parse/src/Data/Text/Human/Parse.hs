{-# LANGUAGE FlexibleInstances, UndecidableInstances, FunctionalDependencies #-}

module Data.Text.Human.Parse
    ( HumanParse (..)
    ) where

import Data.Bool (Bool (..))
import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import Data.Text (Text)
import Data.Void (Void)

import qualified Data.Char as C
import qualified Data.Text as T

-- | An unprincipled best-effort informal attempt to figure out what a human is
-- saying.
--
-- Type variables:
--
--   * @a@ - The type we're trying to parse into
--   * @e@ - The type of error we produce when we can't understand the human
class HumanParse e a | a -> e where

    parseHuman :: Text -> Either e a

instance HumanParse () Void where

    parseHuman _ = Left ()

-- | We parse @'Maybe' a@ by first attempting to parse @a@ and wrap it in
-- @Just@. If that fails but the input is only whitespace, then we accept the
-- as @Nothing@. If there is some /non/-whitespace text that fails to parse as
-- @a@, only then do we reject the input.
instance HumanParse e a => HumanParse e (Maybe a) where

    parseHuman t = case parseHuman t of
        Right a                 -> Right (Just a)
        Left _ | isWhitespace t -> Right Nothing
        Left e                  -> Left e

isWhitespace :: Text -> Bool
isWhitespace = T.all C.isSpace
