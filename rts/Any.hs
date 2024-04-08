{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE PolyKinds             #-}
module Any where

import Language.Haskell.TH
import MemoizedCurry
import Narrowable

mkAllAnyDefinitions :: Int -> Q [Dec]
mkAllAnyDefinitions ar = (\a b c -> a ++ b ++ c) <$>
  mkDataTypes ar <*> mkInstances ar <*> mkFamily ar

mkDataTypes :: Int -> Q [Dec]
mkDataTypes max_ar = concat <$> mapM mkDataType [1..max_ar]

mkDataType :: Int -> Q [Dec]
mkDataType ar = do
    let nm = mkName $ "Any" ++ show ar
    let tvs = map (mkName . ('a':) . show) [1..ar]
    let d = DataD [] nm (map (`PlainTV` ()) tvs) Nothing [] []
    let t = KiSigD nm $ foldr ((AppT . AppT ArrowT) . VarT) StarT tvs
    return [d, t]

mkInstances :: Int -> Q [Dec]
mkInstances max_ar = do
  ds <- dummyInstances
  return $ ds ++ concatMap (\ar -> map (go ar) ds) [1..max_ar]
  where
    go ar (InstanceD _ _ ty ds) =
      let nm = mkName $ "Any" ++ show ar
          tvs = map (mkName . ('a':) . show) [1..ar]
          anyty = foldl AppT (ConT nm) (map VarT tvs)
          updateType (AppT cls _) = AppT cls anyty
          updateType t = t
      in InstanceD Nothing [] (updateType ty) ds
    go ar (TySynInstD (TySynEqn _ lhs _)) =
      let nm = mkName $ "Any" ++ show ar
          anyty = ConT nm
          updateType (AppT cls _) = AppT cls anyty
          updateType t = t
      in TySynInstD (TySynEqn Nothing (updateType lhs) anyty)
    go _ d = d

mkFamily :: Int -> Q [Dec]
mkFamily max_ar = do
  let hd = TypeFamilyHead (mkName "Any") [] NoSig Nothing
  let fam = ClosedTypeFamilyD hd (map mkEqn [1..max_ar])
  let k = mkName "k"
  let sig = KiSigD (mkName "Any")
              (ForallT [PlainTV k SpecifiedSpec] []
              (VarT k))
  return [fam, sig]
  where
    anyHead = AppKindT (ConT (mkName "Any"))
    mkEqn 1 = TySynEqn Nothing (anyHead StarT)
                  (ConT ''None)
    mkEqn ar = TySynEqn Nothing (anyHead (mkArrow ar))
                  (ConT (mkName $ "Any" ++ show ar))
    mkArrow ar = foldr ((AppT . AppT ArrowT) .
                         VarT . mkName . ('a':) . show)
                    StarT [1..ar]

data None

dummyInstances :: Q [Dec]
dummyInstances = [d|
  type instance (HsEquivalent None) = None

  instance ToHs None where
    to = error "FFI Error: 'To' Conversion on ambigouous type variable"

  instance FromHs None where
    from = error "FFI Error: 'From' Conversion on ambigouous type variable"
    elimFlat = error "FFI Error: 'From' Conversion on ambigouous type variable"

  instance HasPrimitiveInfo None where
    primitiveInfo = NoPrimitive

  instance Narrowable None where
    narrow = error "narrowing an ambigouous type variable is not possible"
    narrowConstr _ = error "narrowing an ambigouous type variable is not possible"

  instance NormalForm None where
    nfWith _ _ = error "normalizing an ambigouous type variable is not possible"

  instance Unifiable None where
    unifyWith _ _ _ = error "unifying an ambigouous type variable is not possible"
    lazyUnifyVar _ _ = error "lazily unifying an ambigouous type variable is not possible"

  instance ShowFree None where
    showsFreePrec _ _ = error "showing an ambigouous type variable is not possible"

  instance ShowTerm None where
    showTerm _ _ = error "showing an ambigouous type variable is not possible"

  instance ReadTerm None where
    readTerm = error "reading an ambigouous type variable is not possible"

  instance Levelable None where
    setLevel _ x = x

  instance Curryable None |]
