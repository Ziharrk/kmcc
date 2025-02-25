{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE TypeOperators #-}

import qualified Prelude as P
import qualified BasicDefinitions as B

primuscoreshowTerm_Det# :: B.Curryable a => B.HsEquivalent a -> CList_Det Char_Det
primuscoreshowTerm_Det# x = B.fromForeign P.$ B.showAnyTerm x

primuscoreshowTerm_ND# :: B.Curryable a => B.Curry (a B.:-> CList_ND Char_ND)
primuscoreshowTerm_ND# = P.return P.$ B.Func P.$ \x -> do
  x' <- B.toHaskell x
  P.return (B.from (primuscoreshowTerm_Det# x'))

primuscorereadsUnqualifiedTerm_Det# :: B.Curryable a => CList_Det (CList_Det Char_Det) -> CList_Det Char_Det
                                    -> CList_Det (CTuple2_Det (B.HsEquivalent a) (CList_Det Char_Det))
primuscorereadsUnqualifiedTerm_Det# mdls s = convert P.$ B.readAnyTerm (B.toForeign mdls) (B.toForeign s)
  where
    convert []         = CList_Det
    convert ((a,s):xs) = CCons_Det (CTuple2_Det a (B.fromForeign s)) (convert xs)

primuscorereadsUnqualifiedTerm_ND# :: B.Curryable a => B.Curry (
                                        CList_ND (CList_ND Char_ND)
                                  B.:-> CList_ND Char_ND
                                  B.:-> CList_ND (CTuple2_ND a (CList_ND Char_ND)))
primuscorereadsUnqualifiedTerm_ND# = P.return P.$ B.Func P.$ \mdls -> P.return P.$ B.Func P.$ \s -> do
  mdls' <- B.toHaskell mdls
  s' <- B.toHaskell s
  P.return (B.from (primuscorereadsUnqualifiedTerm_Det# mdls' s'))
