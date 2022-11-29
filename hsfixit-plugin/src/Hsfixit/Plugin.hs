module Hsfixit.Plugin (plugin) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Semigroup qualified as S
import Data.Text qualified as T
import GHC.Builtin.Names
import GHC.Data.Bag qualified as Bag
import GHC.Hs.Extension
import GHC.Parser.Annotation
import GHC.Plugins hiding (TcPlugin)
import GHC.Prelude hiding (span)
import GHC.Rename.Names
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Types.Error
import Hsfixit.Types.Diagnostic qualified as Ty
import Debug.Trace

-- import Language.Haskell.Syntax (ideclExt)

import Data.List (intercalate)
import GHC (IE (..), ImportDecl (..), ieName)
import GHC.Tc.Errors.Types

plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = printTcDiags
    }

sdocContext = defaultSDocContext {sdocStyle = defaultDumpStyle}
dumpSDoc = renderWithContext sdocContext

convertSpan :: SrcSpan -> Ty.Span
convertSpan (RealSrcSpan span _) =
  Ty.Span
    { file = T.pack . unpackFS $ srcSpanFile span
    , startLine = srcSpanStartLine span
    , startCol = srcSpanStartCol span
    , endLine = srcSpanEndLine span
    , endCol = srcSpanEndCol span
    }
--
convertSpan (UnhelpfulSpan _) = error "unhelpfulspan"

convertDiagnostic :: SrcSpan -> TcRnMessage -> Ty.Diagnostic
convertDiagnostic span (TcRnMessageWithInfo _us (TcRnMessageDetailed _ message)) =
  convertDiagnostic span message
convertDiagnostic span e =
  Ty.Diagnostic
    { message = T.pack . showPprUnsafe . unDecorated . diagnosticMessage $ e
    , structured = Nothing
    , span = convertSpan span
    , fixEdits = Nothing
    }

-- FIXME(jade): this will be broken by ghc 9.6 since they obviously changed all
-- this code
--
-- Copied from warnUnusedImport in "GHC.Rename.Names", since the diag is
-- unstructured and this is the point where it becomes so.
importToEdits :: ImportDeclUsage -> Maybe (NonEmpty Ty.Edit)
importToEdits (L loc decl, used, unused)
  -- 'import M()'
  | Just (False, L _ []) <- ideclHiding decl =
      Nothing
  -- import Prelude hiding (..) [Prelude might shadow things in the module even
  -- if Prelude is otherwise unused]
  | Just (True, L _ hides) <- ideclHiding decl
  , not (null hides)
  , pRELUDE_NAME == unLoc (ideclName decl) =
      Nothing
  -- nothing used, delete the whole thing
  | null used =
      Just (Ty.Remove (convertSpan . locA $ loc) :| [])
  -- some number of unused, which should each be deleted
  | Just (_, L _ imports) <- ideclHiding decl =
      let unusedSpans = filter (\(L _loc ie) -> ieName ie `elem` unused) imports
       in NE.nonEmpty $ (Ty.Remove . convertSpan . locA . getLoc) <$> unusedSpans
  | otherwise =
      Nothing

getUnusedImports :: TcGblEnv -> RnM (Maybe (NonEmpty Ty.Edit))
getUnusedImports gbl_env = do
  uses <- readMutVar (tcg_used_gres gbl_env)
  let user_imports =
        filterOut
          (ideclImplicit . unLoc)
          (tcg_rn_imports gbl_env)
  let usage :: [ImportDeclUsage] = findImportUsage user_imports uses
  should <- woptM Opt_WarnUnusedImports
  if should
    then pure . mconcat $ map importToEdits usage
    else pure Nothing

printError :: MsgEnvelope TcRnMessage -> IO ()
printError message = do
  let diag = errMsgDiagnostic message
  putStrLn . showPprUnsafe . unDecorated . diagnosticMessage $ diag
  let hints = diagnosticHints diag
  putStrLn . showPprUnsafe $ hints
  putStrLn . show $ convertDiagnostic (errMsgSpan message) diag
  pure ()

-- putStrLn . conNameOf . errMsgDiagnostic $ message

printTcDiags :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
printTcDiags _cliOpts _modSummary gblEnv = do
  liftIO . putStrLn . show =<< getUnusedImports gblEnv
  env <- getEnv
  let localEnv = env.env_lcl
  errs <- readTcRef localEnv.tcl_errs

  let errList = Bag.bagToList . getMessages $ errs
  liftIO $ mapM_ printError errList
  pure gblEnv
