------------------------------------------------------------------------------
--- A REPL for the KMC compiler.
--- @author  Kai-Oliver Prott
--- @version May 2023
------------------------------------------------------------------------------

module KMCC.ReplConfig where

import Data.List          ( intercalate )
import System.CurryPath   ( inCurrySubdir, modNameToPath, sysLibPath )
import System.FilePath    ( (</>) )
import Installation as I

import REPL.Compiler
import REPL.Main          ( mainREPL )

main :: IO ()
main = mainREPL kmcc

kmccHome :: String
kmccHome = I.installDir

--- The version number
kmccVersion :: (Int,Int,Int)
kmccVersion = (I.majorVersion,I.minorVersion,I.revisionVersion)

--- The subdirectory where intermediate program files and the compiled
--- Go target files will be stored, e.g., `.curry/kmcc-1.0.0`.
kmccSubDir :: String
kmccSubDir =
  ".curry" </> I.compilerName ++ "-" ++
  intercalate "." (map show [maj,min,rev])
 where
  (maj,min,rev) = kmccVersion

------------------------------------------------------------------------------
-- The specification of the KMCC REPL.
kmcc :: CCDescription
kmcc = CCDescription
  I.compilerName                 -- the compiler name
  kmccVersion                    -- the version number
  kmccBanner                     -- the banner
  -- description of specific REPL options:
  [ ("-n|--nocypm",
     "do not invoke `cypm' to compute package load path")
  , ("--noreadline",
     "do not use input line editing via command `rlwrap'")
  ]
  kmccHome                       -- home directory of the compiler
  "info@curry-lang.org"          -- contact email
  (kmccHome </> "bin" </> "kmcc-frontend") -- executable of the Curry front end
  (kmccHome </> "bin" </> "kmcc_c") -- compiler executable
  (kmccHome </> "lib")           -- base library path
  Nothing                        -- compile program with load command
  False                          -- use CURRYPATH variable
  (\s -> "-v" ++ s)              -- option to pass verbosity
  (\s -> "--parse-options \"" ++ s ++ "\"") -- option to pass parser options
  (\s -> "--compile " ++ s)      -- option to compile only
  (\s -> s)                      -- option to create an executable
  cleanCmd                       -- command to clean module
  (CommandLineFreeMode (\vs -> unwords $ map (\(v,i) -> "-V" ++ v ++ "=" ++ show i) vs))
  [ stratOpt, profilingOpt, intOpt, forceOpt
  , ghcOpt -- [firstOpt, resultsOpt, errDepthtOpt]
  ]
 where
  cleanCmd m = unwords
    [ "/bin/rm -f ", quote (kmccSubDir </> m) ++ ".*"
    , quote (kmccSubDir </> "Curry_" ++ m) ++ ".*"
    , quote $ kmccSubDir </> "Curry_" ++ m
    , quote $ modNameToPath m ++ ".curry"
    ]

--- Puts a file argument into quotes to avoid problems with files containing
--- blanks.
quote :: String -> String
quote s = "\"" ++ s ++ "\""

kmccBanner :: String
kmccBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "KMCC Interactive Environment (Version " ++
               I.compilerVersion ++ " of " ++ I.compilerDate ++ ")"
  bannerLine = take (length bannerText) (repeat '-')

forceOpt :: CCOption
forceOpt = CCOption
  "+/-force       "
  "turn on/off forcing of compilation"
  [ ConstOpt "-force" ""
  , ConstOpt "+force" "--force"
  ]

ghcOpt :: CCOption
ghcOpt = CCOption
  "ghc-opts       "
  "pass options to the GHC compiler"
  [ ArgOpt "ghc-opts" "" showOpt ]
  where
    showOpt s = case reads s :: [(String,String)] of
      [(n,"")] -> Just ("--ghc-options \"" ++ s ++ "\"")
      _        -> Nothing

stratOpt :: CCOption
stratOpt = CCOption
  "fs/dfs/bfs     "
  "search strategy (fair / depth-first / breadth-first)"
  [ ConstOpt "fs"  "--fs"
  , ConstOpt "dfs" "--dfs"
  , ConstOpt "bfs" "--bfs"
  ]

profilingOpt :: CCOption
profilingOpt = CCOption
  "+/-profiling   "
  "turn on/off profiling"
  [ ConstOpt "-profiling" ""
  , ConstOpt "+profiling" "--profiling"
  ]

intOpt :: CCOption
intOpt = CCOption
  "+/-interactive "
  "turn on/off interactive evaluation of main expression"
  [ ConstOpt "-interactive" ""
  , ConstOpt "+interactive" "--interactive"
  ]

firstOpt :: CCOption
firstOpt = CCOption
  "+/-first       "
  "turn on/off printing only first value/solution"
  [ ConstOpt "-first" ""
  , ConstOpt "+first" "--first"
  ]

resultsOpt :: CCOption
resultsOpt = CCOption
  "results <n>   "
  "set maximum number of results to be computed\n(default: 0 = infinite)"
  [ ArgOpt "results" "0" showOpt ]
  where
    showOpt s = case reads s :: [(Int,String)] of
      [(n,"")] | n >= 0 -> Just ("--results=" ++ s)
      _                 -> Nothing

errDepthtOpt :: CCOption
errDepthtOpt = CCOption
  "errdepth <n>   "
  "set print depth of expressions in error messages:\nn>0: last n nodes from error point\nn=0: do not print expressions (default)\nn<0: print complete expression"
  [ ArgOpt "errdepth" "0" showOpt ]
  where
    showOpt s = case reads s :: [(Int,String)] of
      [(_,"")] -> Just ("--errdepth=" ++ s)
      _        -> Nothing
