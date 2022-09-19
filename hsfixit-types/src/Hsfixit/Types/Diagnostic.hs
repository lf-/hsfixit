{-# LANGUAGE TemplateHaskell #-}
-- | Extremely boneless version of the new GHC diagnostics infrastructure,
--   suitable for jsonification
module Hsfixit.Types.Diagnostic where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.TH
import Hsfixit.Types.Aeson


data Span = Span
  { file :: !Text
  , startLine :: !Int
  , startCol :: !Int
  , endLine :: !Int
  , endCol :: !Int
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''Span)

data Edit
  = Remove Span
  -- FIXME: probably should be deleted, but aeson will generate a bad
  -- instance without something else here
  | Noop
  deriving stock (Show)

$(deriveJSON taggedOptions ''Edit)

data RedundantImport = RedundantImport {
  } deriving stock (Show)

$(deriveJSON defaultOptions ''RedundantImport)

data StructuredMessage =
  MsgRedundantImport RedundantImport
  | FakeMessage
  deriving stock (Show)

$(deriveJSON taggedOptions ''StructuredMessage)

data Diagnostic = Diagnostic {
  -- | Prettyprinted text message from GHC. If you're regexing this, please
  --   don't, and instead make the plugin emit the structured one.
  message :: Text
  -- | Structured version of the message
  , structured :: Maybe StructuredMessage
  , span :: Span
  } deriving stock (Show)

$(deriveJSON defaultOptions ''Diagnostic)
