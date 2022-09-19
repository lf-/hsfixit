module Hsfixit.Types.Aeson where
import Data.Aeson

taggedOptions :: Options
taggedOptions = defaultOptions {sumEncoding = defaultTaggedObject}

