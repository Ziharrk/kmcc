module CmdParser where

import Data.Maybe ( fromMaybe )
import Data.Version ( showVersion )
import Options.Applicative

import CompilerOpts ( Options(..), KnownExtension, parseOpts, updateOpts )

import Options ( KMCCOpts(..), defaultOpts, defaultFrontendOpts )
import Paths_kmcc (version)

getCmdOpts :: IO KMCCOpts
getCmdOpts = customExecParser (prefs showHelpOnEmpty)
  (info (helper <*> versionParser <*> optParser)
  (progDesc "Compiles Curry files via Haskell"))

versionParser :: Parser (a -> a)
versionParser = infoOption versionText (long "version" <> short 'V' <> help "Show the version number and exit")

versionText :: String
versionText = "KMCC version " ++ showVersion version

optParser :: Parser KMCCOpts
optParser = adjustDefaultOpts
  <$> switch (long "force" <> short 'f' <> help "Force compilation of all files")
  <*> many (strOption (hidden <> long "import-dir" <> short 'i' <> metavar "IDIR" <> help "Add IDIR to the import search path"))
  <*> optional (strOption (hidden <> long "output-dir" <> short 'o' <> metavar "ODIR" <> help "Write output to ODIR"))
  <*> optional (option parseFrontendOpts (hidden <> long "parse-options" <> short 'p' <> metavar "OPTS" <> help "Pass OPTS to the frontend"))
  <*> many (option parseExtension (long "extension" <> short 'X' <> metavar "EXT" <> help "Enable the language extension EXT"
                                <> completeWith (map show [(minBound :: KnownExtension) .. maxBound])))
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
  (_, _:_, _)        -> Left $ "Cannot pass files to frontend via --parse-options"
  where
    fst3 (x, _, _) = x

adjustDefaultOpts :: Bool -> [FilePath] -> Maybe FilePath
                  -> Maybe (Options -> Options) -> [KnownExtension] -> FilePath
                  -> KMCCOpts
adjustDefaultOpts f is o p x t = defaultOpts
  { optTarget = t
  , frontendOpts = adjustFrontendOpts
  }
  where
    adjustFrontendOpts = fromMaybe id p $ defaultFrontendOpts
      { optForce = f
      , optImportPaths = optImportPaths defaultFrontendOpts ++ is
      , optOutDir = fromMaybe (optOutDir defaultFrontendOpts) o
      , optExtensions = optExtensions defaultFrontendOpts ++ x
      }
