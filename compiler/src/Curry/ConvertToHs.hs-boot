{-# LANGUAGE TypeFamilyDependencies #-}
module Curry.ConvertToHs where

import Language.Haskell.Exts ( Context, Name )
import qualified Language.Haskell.Exts as Hs

import Curry.FlatCurry.Type ( TVarWithKind, QName, TypeExpr )

import Curry.ConvertUtils ( HsEquivalent, UnqualName )

mkCurryCtxt :: [TVarWithKind] -> Maybe (Context ())

mkQuantifiedCtxt :: (Hs.Type () -> Hs.Type ()) -> [TVarWithKind] -> Maybe (Context ())

mkFlatPattern :: QName -> TypeExpr -> [Int] -> Hs.Pat ()

class ToHsName a where
  convertTypeNameToHs :: a -> HsEquivalent a
  convertFuncNameToHs :: a -> HsEquivalent a

class ToMonadicHsName a where
  convertTypeNameToMonadicHs :: a -> HsEquivalent a
  convertFuncNameToMonadicHs :: a -> HsEquivalent a

instance ToHsName UnqualName where

instance ToHsName QName where

instance ToMonadicHsName UnqualName where

instance ToMonadicHsName QName where

convertQualNameToFlatName :: QName -> Name ()
convertQualNameToFlatQualName :: QName -> Hs.QName ()

convertTypeToMonadicHs :: TypeExpr -> Hs.Type ()
