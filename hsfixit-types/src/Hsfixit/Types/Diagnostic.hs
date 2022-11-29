{-# LANGUAGE TemplateHaskell #-}
-- | Extremely boneless version of the new GHC diagnostics infrastructure,
--   suitable for jsonification
module Hsfixit.Types.Diagnostic where

import ClassyPrelude
import Data.Aeson
import Data.Aeson.TH
import Data.List.NonEmpty
import Hsfixit.Types.Aeson

-- | This is a serializable and GHC-independent RealSrcSpan
--
--   It uses the same convention for end position: it's the column after the
--   end of the span. For instance, (1, 1)-(1, 2) is length 1
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

data RedundantImport =
  ImportUnused Text
  | ImportItemsUnused Text [Text]
  deriving stock (Show)

$(deriveJSON taggedOptions ''RedundantImport)

data StructuredMessage =
  MsgRedundantImport RedundantImport
  | FakeMessage
  deriving stock (Show)

$(deriveJSON taggedOptions ''StructuredMessage)

-- | FIXME(jade): how do you represent structured messages that *don't* have an
-- unstructured version, for instance if we are the ones generating them?
data Diagnostic = Diagnostic {
  -- | Prettyprinted text message from GHC. If you're regexing this, please
  --   don't, and instead make the plugin emit the structured one.
  message :: Text
  -- | Structured version of the message
  , structured :: Maybe StructuredMessage
  -- | Edits to fix this diagnostic
  , fixEdits :: Maybe (NonEmpty Edit)
  , span :: Span
  } deriving stock (Show)

$(deriveJSON defaultOptions ''Diagnostic)
