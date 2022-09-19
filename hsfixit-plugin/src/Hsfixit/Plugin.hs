module Hsfixit.Plugin (plugin) where
import GHC.Prelude hiding (span)
import Data.Text qualified as T
import GHC.Plugins hiding (TcPlugin)
import Data.Semigroup qualified as S
import GHC.Types.Error
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import Hsfixit.Types.Diagnostic qualified as Ty
import GHC.Data.Bag qualified as Bag
import GHC.Tc.Errors.Types
import Data.List(intercalate)

plugin :: Plugin
plugin = defaultPlugin {
  typeCheckResultAction = printTcDiags
  }

sdocContext = defaultSDocContext {sdocStyle = defaultDumpStyle}
dumpSDoc = renderWithContext sdocContext

parseUnusedImportsStr span message = error $ intercalate "\n" messageStr
  where
  messageStr = dumpSDoc <$> unDecorated message

unknownMessageToDiagnostic :: Diagnostic msg => SrcSpan -> msg -> Ty.Diagnostic
unknownMessageToDiagnostic span message
  | diagnosticReason message == (WarningWithFlag Opt_WarnUnusedImports) = parseUnusedImportsStr span (diagnosticMessage message)

convertSpan (RealSrcSpan span _) = Ty.Span {
  file = T.pack . unpackFS $ srcSpanFile span
  , startLine = srcSpanStartLine span
  , startCol = srcSpanStartCol span
  , endLine = srcSpanEndLine span
  , endCol = srcSpanEndCol span
  }
  --
convertSpan (UnhelpfulSpan _) = error "unhelpfulspan"
convertDiagnostic span (TcRnUnknownMessage message) = unknownMessageToDiagnostic span message
convertDiagnostic span (TcRnMessageWithInfo us (TcRnMessageDetailed _ message)) = convertDiagnostic span message
convertDiagnostic span e = Ty.Diagnostic {message = aa . T.pack . showPprUnsafe . unDecorated . diagnosticMessage $ e, structured = Nothing, span = convertSpan span}
  where
  aa :: T.Text -> T.Text
  aa = ("AAA " S.<>)

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
  env <- getEnv
  let localEnv = env.env_lcl
  errs <- readTcRef localEnv.tcl_errs

  let errList = Bag.bagToList . getMessages $ errs
  liftIO $ mapM_ printError errList
  pure gblEnv

