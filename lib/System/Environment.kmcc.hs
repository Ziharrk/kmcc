{-# LANGUAGE MagicHash #-}

import qualified Prelude as P
import qualified System.Environment as S
import qualified Distribution.System as D
import qualified Network.BSD as N
import BasicDefinitions

environmentdotgetArgs_Det# = fromForeign S.getArgs
environmentdotgetArgs_ND# = P.error "No implementation for getArgs_ND"

environmentdotprimuscoregetEnviron_Det# = liftForeign1 lookup
 where
  lookup v = (S.lookupEnv v P.>>= (\x -> case x of
    P.Nothing -> P.return []
    P.Just y -> P.return y))
environmentdotprimuscoregetEnviron_ND# = P.error "No implementation for getEnviron_ND"


environmentdotprimuscoresetEnviron_Det# = liftForeign2 S.setEnv
environmentdotprimuscoresetEnviron_ND# = P.error "No implementation for setEnviron_ND"

environmentdotprimuscoreunsetEnviron_Det# = liftForeign1 S.unsetEnv
environmentdotprimuscoreunsetEnviron_ND# = P.error "No implementation for unsetEnviron_ND"

environmentdotgetHostname_Det# = fromForeign N.getHostName
environmentdotgetHostname_ND# = P.error "No implementation for getHostname_ND"

environmentdotgetProgName_Det# = fromForeign S.getProgName
environmentdotgetProgName_ND# = P.error "No implementation for getProgName_ND"

environmentdotisWindows_Det# = fromForeign (D.buildOS P.== D.Windows)
environmentdotisWindows_ND# = P.error "No implementation for isWindows_ND"