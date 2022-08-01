------------------------------------------------------------------------------
--- A REPL for the KMC compiler.
--- @author  Kai-Oliver Prott
--- @version July 2022
------------------------------------------------------------------------------

module KMCC.ReplConfig where

import Data.List          ( intercalate )
import System.CurryPath   ( inCurrySubdir, modNameToPath, sysLibPath )
import System.FilePath    ( (</>) )
import KMCC.PkgConfig     ( packagePath )

import REPL.Compiler
import REPL.Main          ( mainREPL )

main :: IO ()
main = mainREPL kmcc

kmccHome :: String
kmccHome = packagePath </> ".."

kmcc :: CCDescription
kmcc = CCDescription
  "kmcc"                         -- the compiler name
  (0,1,0)                        -- the version number
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
  (\s -> "--parse-options" ++ s) -- option to pass parser options
  (\s -> "--compile " ++ s)      -- option to compile only
  (\s -> s)                      -- option to create an executable
  cleanCmd                       -- command to clean module
  (CommandLineFreeMode (\vs -> unwords $ map (\(v,i) -> "-V" ++ v ++ "=" ++ show i) vs))
  [] -- [stratOpt, intOpt, firstOpt, resultsOpt, errDepthtOpt]
 where
  cleanCmd m =
    "/bin/rm -f '" ++ inCurrySubdir m ++ ".*' '" ++ modNameToPath m ++ ".curry'"

kmccBanner :: String
kmccBanner = unlines [bannerLine, bannerText, bannerLine]
 where
  bannerText = "KMCC Interactive Environment"
  bannerLine = take (length bannerText) (repeat '-')

stratOpt :: CCOption
stratOpt = CCOption
  "fs/dfs/bfs     "
  "search strategy (fair / depth-first / breadth-first)"
  [ ConstOpt "fs"  "--fs"
  , ConstOpt "dfs" "--dfs"
  , ConstOpt "bfs" "--bfs"
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