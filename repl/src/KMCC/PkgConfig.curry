module KMCC.PkgConfig where

--- Package version as a string.
packageVersion :: String
packageVersion = "1.0.0"

--- Returns the location (installation directory) of the package.
getPackagePath :: IO String
getPackagePath = return "/home/kaiprott/kmcc/repl/"

--- Package location (deprecated, use 'getPackagePath').
packagePath :: String
packagePath = "/home/kaiprott/kmcc/repl/"

--- Returns the load path for the package (if it is the main package).
getPackageLoadPath :: IO [String]
getPackageLoadPath = do
  pp <- getPackagePath
  return [pp ++ "src", pp ++ ".cpm/packages/io-extra-3.0.0/src", pp ++ ".cpm/packages/read-legacy-3.0.0/src", pp ++ ".cpm/packages/random-3.0.0/src", pp ++ ".cpm/packages/queue-3.0.0/src", pp ++ ".cpm/packages/wl-pprint-3.0.0/src", pp ++ ".cpm/packages/process-3.0.0/src", pp ++ ".cpm/packages/frontend-exec-3.3.0/src", pp ++ ".cpm/packages/propertyfile-3.0.0/src", pp ++ ".cpm/packages/time-3.0.0/src", pp ++ ".cpm/packages/filepath-3.0.0/src", pp ++ ".cpm/packages/directory-3.0.0/src", pp ++ ".cpm/packages/currypath-3.0.0/src", pp ++ ".cpm/packages/abstract-curry-3.0.0/src", pp ++ ".cpm/packages/curry-repl-1.0.0/src"]

--- Load path for the package (deprecated, use 'getPackageLoadPath').
packageLoadPath :: String
packageLoadPath = "/home/kaiprott/kmcc/repl/src:/home/kaiprott/kmcc/repl/.cpm/packages/io-extra-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/read-legacy-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/random-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/queue-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/wl-pprint-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/process-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/frontend-exec-3.3.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/propertyfile-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/time-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/filepath-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/directory-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/currypath-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/abstract-curry-3.0.0/src:/home/kaiprott/kmcc/repl/.cpm/packages/curry-repl-1.0.0/src"

--- Returns the location of the executable installed by this package.
getPackageExecutable :: IO String
getPackageExecutable = return "/home/kaiprott/.cpm/bin/kmcc"

--- Location of the executable (deprecated, use 'getPackageExecutable').
packageExecutable :: String
packageExecutable = "/home/kaiprott/.cpm/bin/kmcc"
