{-# LANGUAGE MagicHash #-}
import qualified BasicDefinitions as BD
import qualified MemoizedCurry  as MC
import qualified Control.Monad.State as S
import Tree

controldotFindalldotallValues_Det# :: BD.Curryable a => BD.HsEquivalent a -> CList_Det (BD.HsEquivalent a)
controldotFindalldotallValues_Det# e = if BD.isBottomNF e
  then CList_Det
  else CCons_Det e CList_Det

controldotFindalldotoneValue_Det# :: BD.Curryable a => BD.HsEquivalent a -> Maybe_Det (BD.HsEquivalent a)
controldotFindalldotoneValue_Det# e = if BD.isBottomNF e
  then Nothing_Det
  else Just_Det e

controldotFindalldotallValues_ND# :: MC.Curryable a => B.Curry (B.LiftedFunc a (CList_ND a))
controldotFindalldotallValues_ND# = BD.returnFunc P.$ \x -> S.get P.>>= \s -> P.return P.$
  toCurryList (bfs (MC.evalCurryTreeWith (BD.groundNormalForm x) s))

controldotFindalldotoneValue_ND# :: MC.Curryable a => B.Curry (B.LiftedFunc a (Maybe_ND a))
controldotFindalldotoneValue_ND# = BD.returnFunc P.$ \x -> S.get P.>>= \s -> P.return P.$
  toCurryMaybe (bfs (MC.evalCurryTreeWith (BD.groundNormalForm x) s))

controldotFindalldotisFail_Det :: a -> CBool_Det
controldotFindalldotisFail_Det _ = CFalse_Det

controldotFindalldotisFail_ND :: MC.Curryable a => B.Curry CBool_ND
controldotFindalldotisFail_ND _ = BD.returnFunc P.$ \x -> S.get P.>>= \s -> P.return P.$
  P.not (P.null (bfs (MC.evalCurryTreeWith (BD.groundNormalForm x) s)))

toCurryList :: [a] -> CList_ND a
toCurryList [] = CList_ND
toCurryList (x:xs) = CCons_ND (P.return x) (P.return (toCurryList xs))

toCurryMaybe :: [a] -> Maybe_ND a
toCurryMaybe [] = Nothing_ND
toCurryMaybe (x:_) = Just_ND (P.return x)
