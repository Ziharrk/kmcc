{-# LANGUAGE TypeFamilies #-}
module Curry.GenInstances (genInstances) where

import Data.List ( isPrefixOf )
import Data.Maybe ( mapMaybe )
import Language.Haskell.Exts hiding ( Cons, QName )
import qualified Language.Haskell.Exts as Hs

import Curry.FlatCurry ( ConsDecl(..), NewConsDecl(..), TypeDecl(..), TypeExpr (..) )

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
      (Just [InsDecl () (FunBind () (concatMap mkShareMatch cs))])
    hsToDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () hsToQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (concatMap mkToMatch cs))])
    hsFromDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () hsFromQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (concatMap mkFromMatch cs))])
    hsNarrowableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () narrowableQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () [mkNarrowMatch]), InsDecl () (FunBind () (concatMap mkSameConstrMatch cs))])
    hsUnifiableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () unifiableQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (concatMap mkUnifyWithMatch cs ++ [unifyWithFailMatch])), InsDecl () (FunBind () (concatMap mkLazyUnifyMatch cs))])
    hsPrimitiveDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () primitiveQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      Nothing
    hsNormalFormDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () normalFormQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (concatMap mkNfWithMatch cs))])
    hsShowFreeDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () showFreeClassQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (concatMap mkShowsFreePrecMatch cs))])
    hsCurryDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () curryClassQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      Nothing

    mkShareMatch (Cons qname2 ar _ _) = Match () (Ident () "shareArgs")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () (mkShareImpl qname2 ar)) Nothing
        : [Match () (Ident () "shareArgs")
            [mkFlatPattern qname2 (TCons qname []) [1..ar]]
            (UnGuardedRhs () (mkShareDetImpl qname2 ar)) Nothing]
    mkToMatch (Cons qname2 ar _ _) = [Match () (Ident () "to")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkToImpl qname2 ar]] ++
      [Match () (Ident () "to")
      [mkFlatPattern qname2 (TCons qname []) [1..ar]]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkToDetImpl qname2 ar]]
    mkFromMatch (Cons qname2 ar _ _) = [Match () (Ident () "from")
      [PApp () (convertTypeNameToHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkFromImpl qname2 ar]]
    mkNarrowMatch = Match () (Ident () "narrow") [] (UnGuardedRhs () (List () (mapMaybe mkNarrowExp cs))) Nothing
    mkNarrowExp (Cons qname2 ar _ _) = preventDict mkNarrowImpl qname2 ar
    mkSameConstrMatch (Cons qname2 ar _ _) = [Match () (Ident () "narrowConstr")
      [PApp () (convertTypeNameToMonadicHs qname2) (replicate ar (PWildCard ()))]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkSameConstrImpl qname2 ar]] ++
      [Match () (Ident () "narrowConstr")
      [mkFlatPattern qname2 (TCons qname []) [1..ar]]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkSameConstrImpl qname2 ar]]
    mkUnifyWithMatch (Cons qname2 ar _ _) = [Match () (Ident () "unifyWith")
      [ PVar () (Ident () "_f")
      , PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . appendName "_a" . indexToName) [1..ar])
      , PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . appendName "_b" . indexToName) [1..ar])
      ]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkUnifyWithImpl qname2 ar]] ++
      [Match () (Ident () "unifyWith")
      [ PVar () (Ident () "_f")
      , mkFlatPattern qname2 (TCons qname []) [1..ar]
      , PVar () (Ident () "_b")
      ]
      (UnGuardedRhs () e) Nothing | ar /= 0, Just e <- [preventDict mkUnifyWithDetImplRight qname2 ar]] ++
      [Match () (Ident () "unifyWith")
      [ PVar () (Ident () "_f")
      , PVar () (Ident () "_a")
      , mkFlatPattern qname2 (TCons qname []) [1..ar]
      ]
      (UnGuardedRhs () e) Nothing | ar /= 0, Just e <- [preventDict mkUnifyWithDetImplLeft qname2 ar]]
    unifyWithFailMatch = Match () (Ident () "unifyWith")
      [PWildCard (), PWildCard (), PWildCard ()]
      (UnGuardedRhs () mkFailed) Nothing
    mkLazyUnifyMatch (Cons qname2 ar _ _) = [Match () (Ident () "lazyUnifyVar")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar]), PVar () (Ident () "_i")]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkLazyUnifyImpl qname2 ar]] ++
      [Match () (Ident () "lazyUnifyVar")
      [mkFlatPattern qname2 (TCons qname []) [1..ar], PVar () (Ident () "_i")]
      (UnGuardedRhs () e) Nothing | ar /= 0, Just e <- [preventDict mkLazyUnifyDetImpl qname2 ar]]
    mkNfWithMatch (Cons qname2 0  _ _) = [Match () (Ident () "nfWith")
      [PVar () (Ident () "_f"), PApp () (convertTypeNameToMonadicHs qname2) []]
      (UnGuardedRhs () $ mkReturn $ mkRight $ Hs.Var () (convertTypeNameToHs qname2)) Nothing]
    mkNfWithMatch (Cons qname2 ar _ _) = [Match () (Ident () "nfWith")
      [PVar () (Ident () "_f"), PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkNfWithImpl qname2 ar]] ++
      [Match () (Ident () "nfWith")
      [PVar () (Ident () "_f"), mkFlatPattern qname2 (TCons qname []) [1..ar]]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkNfWithDetImpl qname2 ar]]
    mkShowsFreePrecMatch (Cons qname2 ar _ _) = [Match () (Ident () "showsFreePrec")
      [PVar () (Ident () "_p"), PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkShowsFreePrecImpl qname2 ar]] ++
      [Match () (Ident () "showsFreePrec")
      [PVar () (Ident () "_p"), mkFlatPattern qname2 (TCons qname []) [1..ar]]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkShowsFreePrecDetImpl qname2 ar]]


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
    mkShareDetImpl qname2 ar =
      mkReturn (App () (Hs.Var () (convertQualNameToFlatQualName qname))
               (foldl (App ()) (Hs.Var () (convertTypeNameToHs qname2))
                 (map (Hs.Var () . UnQual () . indexToName) [1..ar])))
    mkToImpl qname2 ar =
      mkApplicativeChain (Hs.Var () (convertTypeNameToHs qname2))
                          (map (mkToHaskell . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkToDetImpl qname2 ar =
      mkReturn (foldl (App ()) (Hs.Var () (convertTypeNameToHs qname2))
                 (map (Hs.Var () . UnQual () . indexToName) [1..ar]))
    mkFromImpl qname2 ar = App () (Hs.Var () (convertQualNameToFlatQualName qname)) $
      foldl (App ()) (Hs.Var () (convertTypeNameToHs qname2))
        (map (Hs.Var () . UnQual () . indexToName) [1..ar])
    mkNarrowImpl qname2 ar =
      foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
        (map (const mkFree) [1..ar])
    mkSameConstrImpl qname2 ar = foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
      (replicate ar mkFree)
    mkUnifyWithImpl _ ar = Do () $ maybeAddReturnTrue $
      map (\i -> Qualifier () $ App () (App () (Hs.Var () (UnQual () (Ident () "_f")))
                    (Hs.Var () (UnQual () (appendName "_a" (indexToName i)))))
                    (Hs.Var () (UnQual () (appendName "_b" (indexToName i))))) [1..ar]
    mkUnifyWithDetImplRight qname2 ar = mkUnifyWith
      (Hs.Var () (UnQual () (Ident () "_f")))
      (foldl (Hs.App ()) (Hs.Var () $ convertTypeNameToMonadicHs qname2)
        (map (mkFromHaskell . Hs.Var () . UnQual () . indexToName) [1..ar]))
      (Var () (UnQual () (Ident () "_b")))
    mkUnifyWithDetImplLeft qname2 ar = mkUnifyWith
      (Hs.Var () (UnQual () (Ident () "_f")))
      (Var () (UnQual () (Ident () "_a")))
      (foldl (Hs.App ()) (Hs.Var () $ convertTypeNameToMonadicHs qname2)
        (map (mkFromHaskell . Hs.Var () . UnQual () . indexToName) [1..ar]))
    mkLazyUnifyImpl qname2 ar = Do () $
      map (\i -> Generator () (PVar () (appendName "_s" (indexToName i))) (mkShare mkFree)) [1..ar] ++
      [Qualifier () $ mkAddToVarHeap (Hs.Var () (UnQual () (Ident () "_i"))) $ mkReturn
                        (foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
                        (map (Hs.Var () . UnQual () . indexToName) [1..ar]))] ++
      maybeAddReturnTrue (
      map (\i -> Qualifier () $ mkLazyUnify
                    (Hs.Var () (UnQual () (indexToName i)))
                    (Hs.Var () (UnQual () (appendName "_s" (indexToName i))))) [1..ar])
    mkLazyUnifyDetImpl qname2 ar = Do ()
      [ Qualifier () $ mkAddToVarHeap (Hs.Var () (UnQual () (Ident () "_i"))) $ mkReturn $
                        App () (Hs.Var () (convertQualNameToFlatQualName qname))
                        (foldl (App ()) (Hs.Var () (convertTypeNameToHs qname2))
                        (map (Hs.Var () . UnQual () . indexToName) [1..ar]))
      , Qualifier () $ mkReturn (Hs.Var () trueQualName) ]
    mkNfWithImpl qname2 ar = Do () $
      map (\i -> Generator () (PVar () (appendName "_f" (indexToName i))) $
                  App () (Hs.Var () (UnQual () (Ident () "_f"))) $ Hs.Var () $ UnQual () $ indexToName i) [1..ar] ++
      [Qualifier () $ Hs.Case () (mkTuple Unboxed (map (Hs.Var () . UnQual () . appendName "_f" . indexToName) [1..ar]))
        [ Alt () (mkTupleP Unboxed (map (PApp () rightQualName . return . PVar ()  . appendName "_d" . indexToName) [1..ar]))
            (UnGuardedRhs () $ mkReturn $ mkRight $
              foldl (Hs.App ()) (Hs.Var () $ convertTypeNameToHs qname2)
                (map (Hs.Var () . UnQual () . appendName "_d" . indexToName) [1..ar])) Nothing
        , Alt () (PWildCard ())
            (UnGuardedRhs () $ mkReturn $ mkLeft $ mkValUnshared $
              foldl (Hs.App ()) (Hs.Var () $ convertTypeNameToMonadicHs qname2)
                (map (mkEitherToCurry . Hs.Var () . UnQual () . appendName "_f" . indexToName) [1..ar])) Nothing]]
    mkNfWithDetImpl qname2 ar = mkReturn $ mkRight $
      foldl (App ()) (Hs.Var () (convertTypeNameToHs qname2))
        (map (Hs.Var () . UnQual () . indexToName) [1..ar])
    mkShowsFreePrecImpl qname2 0 = mkShowStringCurry (snd qname2)
    mkShowsFreePrecImpl qname2 ar
      | isOpQName qname2 =
        mkShowsBrackets (Hs.Var () (UnQual () (Ident () "_p")))
        (mkShowSpace (mkShowSpace (mkShowsCurryHighPrec (Hs.Var () $ UnQual () $ indexToName 1))
          (mkShowStringCurry (snd qname2)))
          (mkShowsCurryHighPrec (Hs.Var () $ UnQual () $ indexToName 2)))
      | otherwise        =
        mkShowsBrackets (Hs.Var () (UnQual () (Ident () "_p")))
        (foldl1 mkShowSpace (mkShowStringCurry (snd qname2) :
          map (mkShowsCurryHighPrec . Hs.Var () . UnQual () . indexToName) [1..ar]))
    mkShowsFreePrecDetImpl qname2 0 = mkShowStringCurry (snd qname2)
    mkShowsFreePrecDetImpl qname2 ar
      | isOpQName qname2 =
        mkShowsBrackets (Hs.Var () (UnQual () (Ident () "_p")))
        (mkShowSpace (mkShowSpace (mkShowsCurryHighPrec (mkFromHaskell $ Hs.Var () $ UnQual () $ indexToName 1))
          (mkShowStringCurry (snd qname2)))
          (mkShowsCurryHighPrec (mkFromHaskell $ Hs.Var () $ UnQual () $ indexToName 2)))
      | otherwise        =
        mkShowsBrackets (Hs.Var () (UnQual () (Ident () "_p")))
        (foldl1 mkShowSpace (mkShowStringCurry (snd qname2) :
          map (mkShowsCurryHighPrec . mkFromHaskell . Hs.Var () . UnQual () . indexToName) [1..ar]))

    maybeAddReturnTrue [] = [Qualifier () $ mkReturn (Hs.Var () trueQualName)]
    maybeAddReturnTrue xs = xs
genInstances TypeSyn {} = []
genInstances (TypeNew qname1 vis1 vs (NewCons qname2 vis2 ty)) =
  genInstances (Type qname1 vis1 vs [Cons qname2 1 vis2 [ty]])

mkTuple :: Boxed -> [Exp ()] -> Exp ()
mkTuple _       []  = Hs.Con () (Special () (UnitCon()))
mkTuple Boxed   [e] = Hs.App () (Hs.Con () (Qual () (ModuleName () "P") (Ident () "Solo"))) e
mkTuple Unboxed [e] = e
mkTuple boxity  es  = Tuple () boxity es

mkTupleP :: Boxed -> [Pat ()] -> Pat ()
mkTupleP _       []  = PApp () (Special () (UnitCon())) []
mkTupleP Boxed   [p] = PApp () (Qual () (ModuleName () "P") (Ident () "Solo")) [p]
mkTupleP Unboxed [p] = p
mkTupleP boxity  es  = PTuple () boxity es
