{-# LANGUAGE TypeFamilies #-}
module Curry.GenInstances (genInstances) where

import Data.List ( isPrefixOf )
import Data.Maybe ( mapMaybe )
import Language.Haskell.Exts hiding ( Cons, QName )
import qualified Language.Haskell.Exts as Hs

import Curry.FlatCurry ( ConsDecl(..), NewConsDecl(..), TypeDecl(..), TypeExpr (..), TVarWithKind )

import Curry.ConvertUtils
import {-# SOURCE #-} Curry.ConvertToHs

genInstances :: TypeDecl -> [Decl ()]
genInstances (Type _ _ _ []) = []
genInstances (Type qname _ vs cs) =
  (if isListOrTuple qname
    then []
    else [ hsShowDecl, hsReadDecl ])
  ++
  [hsShowFreeDecl | not (isListOrTuple qname)]
  ++
  [ hsEquivDecl 0, hsToDecl, hsFromDecl, hsNarrowableDecl, hsLevelableDecl
  , hsUnifiableDecl, hsPrimitiveDecl, hsNormalFormDecl, hsCurryDecl, hsNFDataDecl ]
  where
    hsNFDataDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () nfDataQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs)))
      )
      (Just [InsDecl () (FunBind () (map mkNFDataMatch cs))])
    mkNFDataMatch (Cons qname2 ar _ _) =
      case preventDict mkNFDataImpl qname2 ar of
        Nothing -> Match () (Ident () "rnfC") [PVar () (Ident () "x")]
          (UnGuardedRhs () (InfixApp () (Var () (UnQual () (Ident () "x"))) (QVarOp () seqQualName) (unit_con ()))) Nothing
        Just e  -> Match () (Ident () "rnfC")
          [PApp () (convertTypeNameToHs qname2) (map (PVar () . indexToName) [1..ar])]
          (UnGuardedRhs () e) Nothing
    mkNFDataImpl _ ar =
      foldr (flip (InfixApp ()) (QVarOp () seqQualName) . (App () (Hs.Var () rnfQualName) . Var () .  UnQual () . indexToName))
        (unit_con ()) [1..ar]

    hsEquivDecl arity = TypeInsDecl () (TyApp () (TyCon () hsEquivQualName)
      (foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
        (map (TyVar () . indexToName . fst) (take arity vs))))
      (foldl (TyApp ()) (TyCon () (convertTypeNameToHs qname))
        (map (TyApp () (TyCon () hsEquivQualName) . TyVar () . indexToName . fst) (take arity vs)))
    hsToDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () hsToQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (concatMap mkToMatch cs))])
    hsFromDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () hsFromQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (concatMap mkFromMatch cs)), InsDecl () (FunBind () (concatMap mkElimFlatMatch cs))])
    hsNarrowableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () narrowableQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () [mkNarrowMatch]), InsDecl () (FunBind () (concatMap mkSameConstrMatch cs))])
    hsLevelableDecl = InstDecl () Nothing
      (IRule () Nothing (mkCurryCtxt vs)
        (IHApp () (IHCon () levelableQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs)))
      )
      (Just [InsDecl () (FunBind () (mkLevelDetMatch : concatMap mkLevelMatch cs))])
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
    hsShowDecl = InstDecl () Nothing
      (IRule () Nothing (if any isDict cs then Nothing else mkShowCtxt vs)
        (IHApp () (IHCon () hsShowQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (concatMap mkShowMatch cs))])
    hsReadDecl = InstDecl () Nothing
      (IRule () Nothing (if any isDict cs then Nothing else mkReadCtxt vs)
        (IHApp () (IHCon () hsReadQualName) (TyParen () $ foldl (TyApp ()) (TyCon () (convertTypeNameToMonadicHs qname))
          (map (TyVar () . indexToName . fst) vs))))
      (Just [InsDecl () (FunBind () (if any isDict cs then [] else mkReadMatch cs))])
    isDict (Cons qname2 _ _ _) = "_Dict#" `isPrefixOf` snd qname2

    mkToMatch (Cons qname2 ar _ _) = [Match () (Ident () "to")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkToImpl qname2 ar]] ++
      [Match () (Ident () "to")
      [mkFlatPattern qname2 (TCons qname []) [1..ar]]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkToDetImpl qname2 ar]]
    mkFromMatch (Cons qname2 ar _ _) = [Match () (Ident () "from")
      [PApp () (convertTypeNameToHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkFromImpl qname2 ar]]
    mkElimFlatMatch (Cons qname2 ar _ _) = [Match () (Ident () "elimFlat")
      [mkFlatPattern qname2 (TCons qname []) [1..ar]]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkElimFlatImpl qname2 ar]]
    mkNarrowMatch = Match () (Ident () "narrow") [] (UnGuardedRhs () (List () (mapMaybe mkNarrowExp cs))) Nothing
    mkNarrowExp (Cons qname2 ar _ _) = preventDict mkNarrowImpl qname2 ar
    mkLevelDetMatch = Match () (Ident () "setLevel")
      [PVar () (Ident () "_l"), PAsPat () (Ident () "_arg") (PApp () (convertQualNameToFlatQualName qname) [PWildCard ()])]
      (UnGuardedRhs () (Hs.Var () (UnQual () (Ident () "_arg")))) Nothing
    mkLevelMatch (Cons qname2 ar _ _) = [Match () (Ident () "setLevel")
      [PVar () (Ident () "_l"), PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkLevelImpl qname2 ar]]
    mkSameConstrMatch c@(Cons qname2 ar _ _) = [Match () (Ident () "narrowConstr")
      [PApp () (convertTypeNameToMonadicHs qname2) (replicate ar (PWildCard ()))]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict (mkSameConstrImpl others) qname2 ar]] ++
      [Match () (Ident () "narrowConstr")
      [mkFlatPattern qname2 (TCons qname []) [1..ar]]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict (mkSameConstrImpl others) qname2 ar]]
      where
        others = filter (/= c) cs
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
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkUnifyWithDetImplRight qname2 ar]] ++
      [Match () (Ident () "unifyWith")
      [ PVar () (Ident () "_f")
      , PVar () (Ident () "_a")
      , mkFlatPattern qname2 (TCons qname []) [1..ar]
      ]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkUnifyWithDetImplLeft qname2 ar]]
    unifyWithFailMatch = Match () (Ident () "unifyWith")
      [PWildCard (), PWildCard (), PWildCard ()]
      (UnGuardedRhs () mkFailed) Nothing
    mkLazyUnifyMatch (Cons qname2 ar _ _) = [Match () (Ident () "lazyUnifyVar")
      [PApp () (convertTypeNameToMonadicHs qname2) (map (PVar () . indexToName) [1..ar]), PVar () (Ident () "_i")]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkLazyUnifyImpl qname2 ar]] ++
      [Match () (Ident () "lazyUnifyVar")
      [mkFlatPattern qname2 (TCons qname []) [1..ar], PVar () (Ident () "_i")]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkLazyUnifyDetImpl qname2 ar]]
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
      [PVar () (Ident () "_p"), PAsPat () (Ident () "_x") (mkFlatPattern qname2 (TCons qname []) [1..ar])]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict mkShowsFreePrecDetImpl qname2 ar]]
    mkShowMatch (Cons qname2 ar _ tys) = [Match () (Ident () "showTerm")
      [PVar () (Ident () "_p"), PApp () (convertTypeNameToHs qname2) (map (PVar () . indexToName) [1..ar])]
      (UnGuardedRhs () e) Nothing | Just e <- [preventDict (mkShowsPrecImpl tys) qname2 ar]]
    mkReadMatch cs' = [Match () (Ident () "readTerm")
      [] (UnGuardedRhs () (mkReadPrecImpl cs')) Nothing]

    preventDict f qname2 ar
      | not ("_Dict#" `isPrefixOf` snd qname2) = Just (f qname2 ar)
      | otherwise = Nothing

    mkToImpl qname2 ar =
      mkApplicativeChain (Hs.Var () (convertTypeNameToHs qname2))
                          (map (mkToHaskell . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkToDetImpl qname2 ar =
      mkReturn (foldl (App ()) (Hs.Var () (convertTypeNameToHs qname2))
                 (map (Hs.Var () . UnQual () . indexToName) [1..ar]))
    mkFromImpl qname2 ar = App () (Hs.Var () (convertQualNameToFlatQualName qname)) $
      foldl (App ()) (Hs.Var () (convertTypeNameToHs qname2))
        (map (Hs.Var () . UnQual () . indexToName) [1..ar])
    mkElimFlatImpl qname2 ar = foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
        (map (mkFromHaskell . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkNarrowImpl qname2 ar =
      mkApplicativeChain (Hs.Var () (convertTypeNameToMonadicHs qname2))
        (map (const (mkShare mkFree)) [1..ar])
    mkLevelImpl qname2 ar =
      foldl (App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2))
        (map (mkSetLevelC (Hs.Var () (UnQual () (Ident () "_l"))) . Hs.Var () . UnQual () . indexToName) [1..ar])
    mkSameConstrImpl others qname2 ar = Tuple () Unboxed [c', others']
      where
        f c a = mkApplicativeChain (Hs.Var () (convertTypeNameToMonadicHs c))
              (replicate a (mkShare mkFree))
        c' = f qname2 ar
        others' = List () $ map (\c -> f (getConsName c) (getConsArity c)) others
        getConsArity (Cons _ ar' _ _) = ar'
        getConsName (Cons qname2' _ _ _) = qname2'
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
            (UnGuardedRhs () $ mkReturn $ mkLeft $ mkVal $
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
    mkShowsFreePrecDetImpl qname2 ar = mkShowsFreePrec (Hs.Var () (UnQual () (Ident () "_p"))) $
                                       (`mkAsTypeOf` Hs.Var () (UnQual () (Ident () "_x"))) $
                                       foldl (Hs.App ()) (Hs.Var () (convertTypeNameToMonadicHs qname2)) $
                                       map (mkFromHaskell . Hs.Var () . UnQual () . indexToName) [1..ar]

    mkShowsPrecImpl _   qname2 0 = mkShowString (snd qname2)
    mkShowsPrecImpl tys qname2 ar = mkShowParen $
      chainShows (
        mkShowString (snd qname2) :
        zipWith (\ty i -> mkShowsPrecHigh ty $ Hs.Var () $ UnQual () $ indexToName i) tys [1..ar])

    mkReadPrecImpl cs' = mkReadParen $ Hs.Paren () $ chainReads $ map mkReadDo cs'
    mkReadDo (Cons qname2 ar _ tys) = Hs.Paren () $ Hs.Do ()
      (mkIdentSymP qname2 : zipWith (\ty i -> mkBindP ty $ PVar () $ indexToName i) tys [1..ar]
      ++ [Qualifier () $ mkReturnP qname2 ar])
    mkIdentSymP q@(_, nm) =
        Generator () (PApp () f
          [PLit () (Signless ()) (String () nm nm)]) (Hs.Var () hsLexPQualName)
      where
        f | isOpQName q = hsSymbolQualName
          | otherwise   = hsIdentQualName
    mkBindP ty p = Generator () p (Hs.App () (Hs.Var () hsReadPrecQualName) (TypeApp () (TyParen () (convertTypeToMonadicHs ty))))
    mkReturnP qname2 ar = mkReturn (foldl (App ()) (Hs.Var () (convertTypeNameToHs qname2))
      (map (Hs.Var () . UnQual () . indexToName) [1..ar]))

    maybeAddReturnTrue [] = [Qualifier () $ mkReturn (Hs.Var () trueQualName)]
    maybeAddReturnTrue xs = xs
genInstances TypeSyn {} = []
genInstances (TypeNew qname1 vis1 vs (NewCons qname2 vis2 ty)) =
  genInstances (Type qname1 vis1 vs [Cons qname2 1 vis2 [ty]])

mkTuple :: Boxed -> [Exp ()] -> Exp ()
mkTuple _       []  = Hs.Con () (Special () (UnitCon ()))
mkTuple Boxed   [e] = Hs.App () (Hs.Con () (Qual () (ModuleName () "P") (Ident () "Solo"))) e
mkTuple Unboxed [e] = e
mkTuple boxity  es  = Tuple () boxity es

mkTupleP :: Boxed -> [Pat ()] -> Pat ()
mkTupleP _       []  = PApp () (Special () (UnitCon ())) []
mkTupleP Boxed   [p] = PApp () (Qual () (ModuleName () "P") (Ident () "Solo")) [p]
mkTupleP Unboxed [p] = p
mkTupleP boxity  es  = PTuple () boxity es

chainShows :: [Exp ()] -> Exp ()
chainShows [] = Hs.Var () (Qual () (ModuleName () "P") (Ident () "id"))
chainShows [e] = e
chainShows (e:es) =
  InfixApp () e (QVarOp () hsDotQualName) $
  InfixApp () (mkShowString " ") (QVarOp () hsDotQualName) $
  chainShows es

chainReads :: [Exp ()] -> Exp ()
chainReads [] = Hs.Var () (Qual () (ModuleName () "P") (Ident () "undefined"))
chainReads [e] = e
chainReads (e:es) =
  InfixApp () e (QVarOp () hsConcatRead) $
  chainReads es

hsShowQualName :: Hs.QName ()
hsShowQualName = Qual () (ModuleName () "B") (Ident () "ShowTerm")

hsReadQualName :: Hs.QName ()
hsReadQualName = Qual () (ModuleName () "B") (Ident () "ReadTerm")

hsShowStringQualName :: Hs.QName ()
hsShowStringQualName = Qual () (ModuleName () "P") (Ident () "showString")

hsShowParenQualName :: Hs.QName ()
hsShowParenQualName = Qual () (ModuleName () "P") (Ident () "showParen")

hsShowsPrecQualName :: Hs.QName ()
hsShowsPrecQualName = Qual () (ModuleName () "B") (Ident () "showTerm")

hsReadPrecQualName :: Hs.QName ()
hsReadPrecQualName = Qual () (ModuleName () "B") (Ident () "readTerm")

hsNeqQualName :: Hs.QName ()
hsNeqQualName = Qual () (ModuleName () "P") (Symbol () "/=")

hsDotQualName :: Hs.QName ()
hsDotQualName = Qual () (ModuleName () "P") (Symbol () ".")

hsConcatRead :: Hs.QName ()
hsConcatRead = Qual () (ModuleName () "P") (Symbol () "+++")

hsReadParenQualName :: Hs.QName ()
hsReadParenQualName = Qual () (ModuleName () "P") (Ident () "parens")

hsLexPQualName :: Hs.QName ()
hsLexPQualName = Qual () (ModuleName () "P") (Ident () "lexP")

hsSymbolQualName :: Hs.QName ()
hsSymbolQualName = Qual () (ModuleName () "P") (Ident () "Symbol")

hsIdentQualName :: Hs.QName ()
hsIdentQualName = Qual () (ModuleName () "P") (Ident () "Ident")

mkShowString :: String -> Exp ()
mkShowString s = App () (Hs.Var () hsShowStringQualName) $ Hs.Lit () $ String () s s

mkShowParen :: Exp () -> Exp ()
mkShowParen = App () (App () (Hs.Var () hsShowParenQualName)
  (InfixApp ()
    (Hs.Var () (UnQual () (Ident () "_p")))
    (QVarOp () hsNeqQualName)
    (Hs.Lit () (Int () 0 "0"))))

mkReadParen :: Exp () -> Exp ()
mkReadParen = App () (Hs.Var () hsReadParenQualName)

mkShowsPrecHigh :: TypeExpr -> Exp () -> Exp ()
mkShowsPrecHigh ty = App () (App () (App () (Hs.Var () hsShowsPrecQualName) (TypeApp () (TyParen () (convertTypeToMonadicHs ty)))) (Hs.Lit () (Int () 9 "9")))

mkShowCtxt :: [TVarWithKind] -> Maybe (Context ())
mkShowCtxt = mkQuantifiedCtxt (TyApp () (TyCon () hsShowQualName))

mkReadCtxt :: [TVarWithKind] -> Maybe (Context ())
mkReadCtxt = mkQuantifiedCtxt (TyApp () (TyCon () hsReadQualName))

mkSetLevelC :: Exp () -> Exp () -> Exp ()
mkSetLevelC = App () . App () (Hs.Var () (Qual () (ModuleName () "B") (Ident () "setLevelC")))
