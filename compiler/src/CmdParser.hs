module CmdParser where

import Data.Maybe ( fromMaybe )
import Options.Applicative

import CompilerOpts ( Options(..), KnownExtension, parseOpts, updateOpts, Verbosity (..) )

import Options ( KMCCOpts(..), defaultOpts, defaultFrontendOpts )

getCmdOpts :: IO KMCCOpts
getCmdOpts = customExecParser (prefs showHelpOnEmpty)
  (info (helper <*> optParser)
  (progDesc "Compiles Curry files via Haskell"))

optParser :: Parser KMCCOpts
optParser = adjustDefaultOpts
  <$> switch (long "force" <> short 'f' <> help "Force compilation of all files")
  <*> switch (long "compile" <> short 'c' <> help "Compile only, do not generate an executable")
  <*> switch (long "quiet" <> short 'q' <> help "Suppress all output (equivalent to -v0, overwritten by -vn with n >= 1)")

  <*> switch (long "compiler-name" <> help "Print the compiler name (kmcc) and exit")
  <*> switch (long "numeric-version" <> help "Print the compiler version and exit")
  <*> switch (long "base-version" <> help "Print the base package version for this compiler and exit")

  <*> optional (option parseVerbosity (long "verbose" <> short 'v' <> metavar "n" <> help "Set verbosity level to n, with 0 <= n <= 4, default = 1"))
  <*> many (strOption (hidden <> long "import-dir" <> short 'i' <> metavar "IDIR" <> help "Add IDIR to the import search path"))
  <*> optional (strOption (hidden <> long "output-dir" <> short 'o' <> metavar "ODIR" <> help "Write output to ODIR"))
  <*> optional (option parseFrontendOpts (hidden <> long "parse-options" <> short 'p' <> metavar "OPTS" <> help "Pass OPTS to the frontend"))
  <*> many (option parseExtension (long "extension" <> short 'X' <> metavar "EXT" <> help "Enable the language extension EXT"
                                <> completeWith (map show [(minBound :: KnownExtension) .. maxBound])))

  <*> switch (long "disable-det-optimization" <> help "Disable optimization of deterministic programs (not included in -O0)")
  <*> optional (option parseOptimization (long "optimization" <> short 'O' <> metavar "n" <> help "Set optimization level to n, with 0 <= n <= 2, default = 1"))

  <*> argument str (metavar "FILE" <> help "Curry file to compile" <> completer (bashCompleter "file"))

parseExtension :: ReadM KnownExtension
parseExtension = eitherReader $ \s -> case reads s of
  [(e, "")] -> return e
  _         -> Left $ "Invalid extension: " ++ s

parseFrontendOpts :: ReadM (Options -> Options)
parseFrontendOpts = eitherReader $ \s -> case parseOpts (words s) of
  -- to circumvent the bad frontend api for parsing options,
  -- we parse them first to check for errors and throw away the result.
  -- Instead of returning the parsed options,
  -- we return a function that adds the parsed options to the given options.
  (_, [], [])        -> return (\opts -> fst3 $ updateOpts opts (words s))
  (_, _, errs@(_:_)) -> Left $ "Invalid frontend options: " ++ unlines errs
  (_, _:_, _)        -> Left "Cannot pass files to frontend via --parse-options"
  where
    fst3 (x, _, _) = x

parseVerbosity :: ReadM Int
parseVerbosity = eitherReader $ \s -> case reads s of
  [(v, "")]
    | v >= 0 && v <= 4 -> return v
  _                    -> Left $ "Invalid verbosity level (0 <= n <= 4): " ++ s

parseOptimization :: ReadM Int
parseOptimization = eitherReader $ \s -> case reads s of
  [(o, "")]
    | o >= 0 && o <= 2 -> return o
  _                    -> Left $ "Invalid optimization level (0 <= n <= 2): " ++ s

adjustDefaultOpts :: Bool -> Bool -> Bool
                  -> Bool -> Bool -> Bool
                  -> Maybe Int -> [FilePath] -> Maybe FilePath -> Maybe (Options -> Options) -> [KnownExtension]
                  -> Bool -> Maybe Int
                  -> FilePath
                  -> KMCCOpts
adjustDefaultOpts f c q cn cv bv v is o p x dOpt opt t = defaultOpts
  { optTarget = t
  , optCompilerVerbosity = verbosity
  , optCompileOnly = c
  , optInfo = (cn, cv, bv)
  , optOptimizationBaseLevel = fromMaybe (optOptimizationBaseLevel defaultOpts) opt
  , optOptimizationDeterminism = not dOpt && optOptimizationDeterminism defaultOpts
  , frontendOpts = adjustFrontendOpts
  }
  where
    verbosity = fromMaybe (if q then 0 else 1) v
    adjustFrontendOpts = fromMaybe id p $ defaultFrontendOpts
      { optForce = f
      , optVerbosity = if verbosity == 0 then VerbQuiet else VerbStatus
      , optImportPaths = optImportPaths defaultFrontendOpts ++ is
      , optOutDir = fromMaybe (optOutDir defaultFrontendOpts) o
      , optExtensions = optExtensions defaultFrontendOpts ++ x
      }
