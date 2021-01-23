--

-- | Let's go lambdabot!
module Main where

import Lambdabot.Main (
  Config,
  DSum,
  Priority (
    ALERT,
    CRITICAL,
    DEBUG,
    EMERGENCY,
    ERROR,
    INFO,
    NOTICE,
    WARNING
  ),
  consoleLogLevel,
  dataDir,
  enableInsults,
  lambdabotMain,
  lbVersion,
  onStartupCmds,
  (==>),
 )

import Control.Monad.Identity (Identity, unless, void, when, (<=<))
import Data.Char (toUpper)
import Data.Version (showVersion)
import Modules (modulesInfo)
import qualified Paths_lambdabot as P
import System.Console.GetOpt (
  ArgDescr (NoArg, ReqArg),
  ArgOrder (Permute),
  OptDescr (Option),
  getOpt,
  usageInfo,
 )
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitSuccess, exitWith)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)

strs :: a -> IO [a]
strs = return . (: [])

flagsOptionHelp :: OptDescr (IO (DSum Config Identity))
flagsOptionHelp =
  Option "h?" ["help"] (NoArg (usage [])) "Print this help message"

flagsOptionEval :: OptDescr (IO (DSum Config Identity))
flagsOptionEval =
  Option
    "e"
    []
    (arg "<command>" onStartupCmds strs)
    "Run a lambdabot command instead of a REPL"

flagsOptionVersion :: OptDescr (IO (DSum Config Identity))
flagsOptionVersion =
  Option "V" ["version"] (NoArg version) "Print the version of lambdabot"

flagsOptionNice :: OptDescr (IO (DSum Config Identity))
flagsOptionNice =
  Option
    "n"
    ["nice"]
    (NoArg noinsult)
    "Be nice (disable insulting error messages)"
 where
  noinsult = return (enableInsults ==> False)

flagsOptionLogLevel :: OptDescr (IO (DSum Config Identity))
flagsOptionLogLevel =
  Option
    "l"
    []
    (arg "<level>" consoleLogLevel level)
    "Set the logging level"
 where
  level str = case reads (map toUpper str) of
    (lv, []) : _ -> return lv
    _ ->
      usage
        [ "Unknown log level."
        , "Valid levels are: "
            ++ show
              [DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY]
        ]

-- flagsOptionTrustPackage       = Option "t"  ["trust"] (arg "<package>" trustedPackages strs)  "Trust the specified packages when evaluating code"
-- flagsOptionLanguageExtensions = Option "X"  []        (arg "<extension>" languageExts strs)   "Set a GHC language extension for @run"

arg ::
  String ->
  Config t ->
  (String -> IO t) ->
  ArgDescr (IO (DSum Config Identity))
arg descr key fn = ReqArg (fmap (key ==>) . fn) descr

flags :: [OptDescr (IO (DSum Config Identity))]
flags =
  [ flagsOptionHelp
  , flagsOptionEval
  , flagsOptionLogLevel
  , {- flagsOptionTrustPackage, flagsOptionLanguageExtensions, -}
    flagsOptionVersion
  , flagsOptionNice
  ]

versionString :: String
versionString = "lambdabot version " ++ showVersion P.version

version :: IO a
version = do
  putStrLn versionString
  exitSuccess

usage :: [String] -> IO a
usage errors = do
  cmd <- getProgName

  let isErr = not (null errors)
      out = if isErr then stderr else stdout

  mapM_ (hPutStrLn out) errors
  when isErr (hPutStrLn out "")

  hPutStrLn out versionString
  hPutStr out (usageInfo (cmd ++ " [options]") flags)

  exitWith (if isErr then ExitFailure 1 else ExitSuccess)

-- do argument handling
main :: IO ()
main = do
  (config, nonOpts, errors) <- getOpt Permute flags <$> getArgs
  unless (null errors && null nonOpts) (usage errors)
  config' <- sequence config
  dir <- P.getDataDir
  exitWith
    <=< lambdabotMain modulesInfo
    $ [dataDir ==> dir, lbVersion ==> P.version]
      ++ config'

-- special online target for ghci use
online :: [String] -> IO ()
online startUpCommands = do
  dir <- P.getDataDir
  void $
    lambdabotMain
      modulesInfo
      [ dataDir ==> dir
      , lbVersion ==> P.version
      , onStartupCmds ==> startUpCommands
      ]
