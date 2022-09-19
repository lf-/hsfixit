module Hsfixit.Parse where
import GHC.Prelude
import Data.Attoparsec.Text
import Data.Text (Text)

data UnusedImports

parseUnusedImports :: Text -> Parser UnusedImports
parseUnusedImports = undefined
