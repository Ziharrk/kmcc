{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified System.Info as S
import qualified Data.Version as D
import BasicDefinitions

compilerdotDistributiondotcurryCompiler_Det# = P.error "No implementation of curryCompiler"
compilerdotDistributiondotcurryCompiler_ND# = P.error "No implementation of curryCompile_ND"

compilerdotDistributiondotcurryCompilerMajorVersion_Det# = P.error "No implementation of curryCompilerMajorVersion"
compilerdotDistributiondotcurryCompilerMajorVersion_ND# = P.error "No implementation of curryCompilerMajorVersion_ND"

compilerdotDistributiondotcurryCompilerMinorVersion_Det# = P.error "No implementation of curryCompilerMinorVersion"
compilerdotDistributiondotcurryCompilerMinorVersion_ND# = P.error "No implementation of curryCompilerMinorVersion_ND"

compilerdotDistributiondotcurryCompilerRevisionVersion_Det# = P.error "No implementation of curryCompilerRevisionVersion"
compilerdotDistributiondotcurryCompilerRevisionVersion_ND# = P.error "No implementation of curryCompilerRevisionVersion_ND"

compilerdotDistributiondotcurryRuntime_Det# = fromForeign S.compilerName
compilerdotDistributiondotcurryRuntime_ND# = P.error "No implementation of curryRuntime_ND"

compilerdotDistributiondotcurryRuntimeMajorVersion_Det# = fromForeign P.$ P.toInteger P.$ (D.versionBranch S.compilerVersion) P.!! 0
compilerdotDistributiondotcurryRuntimeMajorVersion_ND# = P.error "No implementation of curryRuntimeMajorVersion_ND"

compilerdotDistributiondotcurryRuntimeMinorVersion_Det# = fromForeign P.$ P.toInteger P.$ (D.versionBranch S.compilerVersion) P.!! 1
compilerdotDistributiondotcurryRuntimeMinorVersion_ND# = P.error "No implementation of curryRuntimeMinorVersion_ND"

compilerdotDistributiondotbaseVersion_Det# = P.error "No implementation of baseVersion"
compilerdotDistributiondotbaseVersion_ND# = P.error "No implementation of baseVersion_ND"

compilerdotDistributiondotinstallDir_Det# = P.error "No implementation of installDir"
compilerdotDistributiondotinstallDir_ND# = P.error "No implementation of installDir_ND"