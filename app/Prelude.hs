module Prelude
    ( module Prelude
    , module Data.ByteString
    , module Data.Text
    )
where

import Data.ByteString (ByteString)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import "base" Prelude hiding (readFile)

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

fromText :: (IsString s) => Text -> s
fromText = fromString . Text.unpack
