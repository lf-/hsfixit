module Hsfixit.Plugin (plugin) where
import GHC.Prelude
import GHC.Plugins hiding (TcPlugin)
import GHC.Utils.Logger
import GHC.Tc.Types
import GHC.Tc.Utils.Monad

plugin :: Plugin
plugin = defaultPlugin {
  typeCheckResultAction = printTcDiags
  }

printTcDiags :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
printTcDiags _cliOpts _modSummary gblEnv = do
  env <- getEnv
  let localEnv = env.env_lcl
  errs <- readTcRef localEnv.tcl_errs

  -- holy fuck this is evil
  liftIO . putStrLn . showPprUnsafe $ errs
  pure gblEnv

