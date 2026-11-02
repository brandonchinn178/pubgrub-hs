{-# LANGUAGE DataKinds #-}

module PubGrub.Logger (
  Logger (..),
  LogLevel (..),
) where

import Data.Text (Text)
import GHC.Records (HasField (..))
import Prelude hiding (log)

newtype Logger m = Logger (LogLevel -> Text -> m ())

data LogLevel
  = LevelDebug
  | LevelInfo
  | LevelWarn
  | LevelError
  deriving (Show, Eq)

instance HasField "debug" (Logger m) (Text -> m ()) where
  getField (Logger log) = log LevelDebug
instance HasField "info" (Logger m) (Text -> m ()) where
  getField (Logger log) = log LevelInfo
instance HasField "warn" (Logger m) (Text -> m ()) where
  getField (Logger log) = log LevelWarn
instance HasField "error" (Logger m) (Text -> m ()) where
  getField (Logger log) = log LevelError
