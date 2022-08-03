{-# LANGUAGE TypeFamilyDependencies #-}
module Curry.ConvertToHs where

import Language.Haskell.Exts ( Context )

import Curry.FlatCurry.Type ( TVarWithKind, QName )

import Curry.ConvertUtils ( HsEquivalent, UnqualName )

mkCurryCtxt :: [TVarWithKind] -> Maybe (Context ())

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
