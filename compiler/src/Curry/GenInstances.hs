{-# LANGUAGE TypeFamilies #-}
module Curry.GenInstances (genInstances) where

import Data.List ( isPrefixOf )
import Data.Maybe ( mapMaybe )
import Language.Haskell.Exts hiding ( Cons, QName )
import qualified Language.Haskell.Exts as Hs

import Curry.FlatCurry ( ConsDecl(..), NewConsDecl(..), TypeDecl(..) )

import Curry.ConvertUtils
import {-# SOURCE #-} Curry.ConvertToHs

genInstances :: TypeDecl -> [Decl ()]
genInstances (Type _ _ _ []) = []
genInstances (Type qname _ vs cs) =
  [hsShowFreeDecl | not (isListOrTuple qname)]
  ++
  map hsEquivDecl [0..length vs] ++
  [ shareableDecl, hsToDecl, hsFromDecl, hsNarrowableDecl
  , hsUnifiableDecl, hsPrimitiveDecl, hsNormalFormDecl, hsCurryDecl ]
  where
    hsEquivDecl arity = TypeInsDecl () (TyApp () (TyCon () hsEquivQualName)
      (foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
        (map (TyVar () . indexToName . fst) (take arity vs))))
      (foldl (TyApp ()) (TyCon () (convertTypeNameToHs qname))
        (map (TyApp () (TyCon () hsEquivQualName) . TyVar () . indexToName . fst) (take arity vs)))
    shareableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHApp () (IHCon () shareableQualName) (TyCon () curryQualName)) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (map mkShareMatch cs))])
    hsToDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () hsToQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkToMatch cs))])
    hsFromDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () hsFromQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkFromMatch cs))])
    hsNarrowableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () narrowableQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () [mkNarrowMatch]), InsDecl () (FunBind () (mapMaybe mkSameConstrMatch cs))])
    hsUnifiableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () unifiableQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkUnifyWithMatch cs ++ [unifyWithFailMatch])), InsDecl () (FunBind () (mapMaybe mkLazyUnifyMatch cs))])
    hsPrimitiveDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () primitiveQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      Nothing
    hsNormalFormDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () normalFormQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkNfWithMatch cs))])
    hsShowFreeDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () showFreeClassQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (mapMaybe mkShowsFreePrecMatch cs))])
    hsCurryDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () curryClassQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      Nothing
    mkShareMatch (Cons qname2 ar _ _) = Match () (Ident () "shareArgs")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () (mkShareImpl qname2 ar)) Nothing
    mkToMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "to")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (preventDict mkToImpl qname2 ar)
    mkFromMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "from")
      [PApp () (convertTypeNameToHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (preventDict mkFromImpl qname2 ar)
    mkNarrowMatch = Match () (Ident () "narrow") [] (UnGuardedRhs () (List () (mapMaybe mkNarrowExp cs))) Nothing
    mkNarrowExp (Cons qname2 ar _ _) = preventDict mkNarrowImpl qname2 ar
    mkSameConstrMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "narrowConstr")
      [PApp () (convertTypeNameToMonadicHs qname2) (replicate ar (PWildCard ()))]
      (UnGuardedRhs () e) Nothing) (preventDict mkSameConstrImpl qname2 ar)
    mkUnifyWithMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "unifyWith")
      [ PVar () (Ident () "_f")
      , PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () .  appendName "_a" . indexToName) [1..ar])
      , PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () .  appendName "_b" . indexToName) [1..ar])
      ]
      (UnGuardedRhs () e) Nothing) (preventDict mkUnifyWithImpl qname2 ar)
    unifyWithFailMatch = Match () (Ident () "unifyWith")
      [PWildCard (), PWildCard (), PWildCard ()]
      (UnGuardedRhs () mkFailed) Nothing
    mkLazyUnifyMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "lazyUnifyVar")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar]), PVar () (Ident () "_i")]
      (UnGuardedRhs () e) Nothing) (preventDict mkLazyUnifyImpl qname2 ar)
    mkNfWithMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "nfWith")
      [PVar () (Ident () "_f"), PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (preventDict mkNfWithImpl qname2 ar)
    mkShowsFreePrecMatch (Cons qname2 ar _ _) = fmap (\e -> Match () (Ident () "showsFreePrec")
      [PVar () (Ident () "_p"), PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing) (preventDict mkShowsFreePrecImpl qname2 ar)
    preventDict f qname2 ar
      | not ("_Dict#" `isPrefixOf` snd qname2) = Just (f qname2 ar)
      | otherwise = Nothing
    mkShareImpl qname2 ar
      | not ("_Dict#" `isPrefixOf` snd qname2) =
        mkApplicativeChain (Hs.Var () (convertTypeNameToMonadicHs qname2))
                           (map (mkShare . Hs.Var () . UnQual () . indexToName) [1..ar])
      | otherwise =
        mkReturn (foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
                   (map (Hs.Var () . UnQual () . indexToName) [1..ar]))
    mkToImpl qname2 ar =
      mkApplicativeChain (Hs.Var () (convertTypeNameToHs qname2))
                          (map (mkToHaskell . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkFromImpl qname2 ar =
      foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
        (map (mkFromHaskell . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkNarrowImpl qname2 ar =
      foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
        (map (const mkFree) [1..ar])
    mkSameConstrImpl qname2 ar = foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
      (replicate ar mkFree)
    mkUnifyWithImpl _ ar = Do () $ maybeAddReturnTrue $
      map (\i -> Qualifier () $ App () (App () (Hs.Var () (UnQual () (Ident () "_f")))
                    (Hs.Var () (UnQual () (appendName "_a" (indexToName i)))))
                    (Hs.Var () (UnQual () (appendName "_b" (indexToName i))))) [1..ar]
    mkLazyUnifyImpl qname2 ar = Do () $
      map (\i -> Generator () (PVar () (appendName "_s" (indexToName i))) (mkShare mkFree)) [1..ar] ++
      [Qualifier () $ mkAddToVarHeap (Hs.Var () (UnQual () (Ident () "_i"))) $ mkReturn
                        (foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
                        (map (Hs.Var () . UnQual () . indexToName) [1..ar]))] ++
      maybeAddReturnTrue (
      map (\i -> Qualifier () $ mkLazyUnify
                    (Hs.Var () (UnQual () (indexToName i)))
                    (Hs.Var () (UnQual () (appendName "_s" (indexToName i))))) [1..ar])
    mkNfWithImpl qname2 ar = mkApplicativeChain
      (mkLambda (map (PVar () . appendName "_l" . indexToName) [1..ar])
                (foldl (\c -> App () c . mkReturn . Hs.Var (). UnQual () . appendName "_l" . indexToName)
                       (Hs.Var () (convertTypeNameToMonadicHs qname2))
                       [1..ar]))
      (map (App () (Hs.Var () (UnQual () (Ident () "_f"))) . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkShowsFreePrecImpl qname2 0 = mkShowStringCurry (snd qname2)
    mkShowsFreePrecImpl qname2 ar
      | isOpQName qname2 =
        mkShowsBrackets (Hs.Var () (UnQual () (Ident () "_p")))
        (mkShowSpace (mkShowSpace (mkShowsCurryHighPrec (indexToName 1)) (mkShowStringCurry (snd qname2)))
          (mkShowsCurryHighPrec (indexToName 2)))
      | otherwise        =
        mkShowsBrackets (Hs.Var () (UnQual () (Ident () "_p")))
        (foldl1 mkShowSpace (mkShowStringCurry (snd qname2) : map (mkShowsCurryHighPrec . indexToName) [1..ar]))
    maybeAddReturnTrue [] = [Qualifier () $ mkReturn (Hs.Var () trueQualName)]
    maybeAddReturnTrue xs = xs
    mkLambda [] e = e
    mkLambda ps e = Paren () (Hs.Lambda () ps e)
genInstances TypeSyn {} = []
genInstances (TypeNew qname1 vis1 vs (NewCons qname2 vis2 ty)) =
  genInstances (Type qname1 vis1 vs [Cons qname2 1 vis2 [ty]])
