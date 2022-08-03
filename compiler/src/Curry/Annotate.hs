module Curry.Annotate (annotateND, isFunFree, exprAnn) where

import Control.Arrow (second)
import qualified Data.Map as Map

import Curry.FlatCurry.Typeable (Typeable(..))
import Curry.FlatCurry.Typed.Type (TExpr (..), TBranchExpr (..), TPattern (..), TypeExpr (..))
import Curry.FlatCurry.Annotated.Type (AExpr(..), ABranchExpr (..), APattern(..))

import Curry.Analysis (NDInfo(..), NDAnalysisResult)

annotateND :: NDAnalysisResult -> TExpr -> AExpr (TypeExpr, NDInfo)
annotateND analysis = annotateND' analysis Map.empty

annotateND' :: NDAnalysisResult -> Map.Map Int NDInfo -> TExpr -> AExpr (TypeExpr, NDInfo)
annotateND' _ vMap (TVarE ty x) = AVar (ty, Map.findWithDefault NonDet x vMap) x
annotateND' _ _ (TLit ty l) = ALit (ty, Det) l
annotateND' analysis vMap (TComb ty ct qname args) =
  AComb (ty, ann) ct (qname, (ty', annHere)) args'
  where
    argTys = map typeOf args
    ty' = foldr FuncType ty argTys
    annHere = case Map.lookup qname analysis of
                Nothing -> if all isFunFree argTys then Det else NonDet
                Just det -> det
    args' = map (annotateND' analysis vMap) args
    ann = maximum (annHere : map (snd . exprAnn) args')
annotateND' analysis vMap (TFree bs e) =
  AFree (typeOf e, NonDet) (map (second (,NonDet)) bs) (annotateND' analysis vMap' e)
  where vMap' = Map.union vMap (Map.fromList (map ((,NonDet) . fst) bs))
annotateND' analysis vMap (TOr e1 e2) =
  AOr (typeOf e1, NonDet) (annotateND' analysis vMap e1) (annotateND' analysis vMap e2)
annotateND' analysis vMap ex@(TCase ct e alts) =
  ACase (typeOf ex, ann) ct e' alts'
  where
    annScr = snd $ exprAnn e'
    ann = maximum (annScr : map (snd . altAnn) alts')
    e' = annotateND' analysis vMap e
    alts' = concatMap (annotateAlt analysis vMap annScr) alts
annotateND' analysis vMap (TTyped e ty) =
  let e' = annotateND' analysis vMap e
  in  ATyped (exprAnn e') e' ty
annotateND' analysis vMap (TLet bs e) =
  ALet (typeOf e, ann) bs' e' -- TODO passing of vMap is overly conservative by not updating
  where
    e' = annotateND' analysis vMap e
    ann = maximum (snd (exprAnn e') : map (snd . snd . fst) bs')
    bs' = map annBind bs
    annBind ((x, _), e2) =
      let e2' = annotateND' analysis vMap e2
      in ((x, exprAnn e2'), e2')

annotateAlt :: NDAnalysisResult -> Map.Map Int NDInfo -> NDInfo -> TBranchExpr
            -> [ABranchExpr (TypeExpr, NDInfo)]
annotateAlt analysis vMap ann (TBranch x e) =
  case altAnn normalBranch of
    (_, Det)    -> [normalBranch]
    (_, NonDet) -> [normalBranch, alternativeBranch]
  where
    normalBranch = ABranch (annotatePat x) (annotateND' analysis vMapNormal e)
    alternativeBranch = ABranch (annotatePat (toAlternativePat x)) (annotateND' analysis vMapAlternative e)
    vMapNormal = Map.union vMap (Map.fromList (map ((,ann) . fst) (patArgs x)))
    vMapAlternative = Map.union vMap (Map.fromList (map ((,Det) . fst) (patArgs x)))
    patArgs (TPattern _ _ args) = args
    patArgs (TLPattern _ _)     = []
    toAlternativePat (TPattern ty qname args) = TPattern ty (second (++ "#Det") qname) args
    toAlternativePat p = p

annotatePat :: TPattern -> APattern (TypeExpr, NDInfo)
annotatePat (TPattern ty qname args) = APattern (ty, Det) (qname, (ty, Det)) (map (second (,NonDet)) args)
annotatePat (TLPattern ty l) = ALPattern (ty, Det) l

isFunFree :: TypeExpr -> Bool
isFunFree (FuncType _ _) = False
isFunFree (TVar _) = False
isFunFree (ForallType _ ty) = isFunFree ty
isFunFree (TCons _ args) = all isFunFree args

exprAnn :: AExpr a -> a
exprAnn (AVar a _) = a
exprAnn (ALit a _) = a
exprAnn (AComb a _ _ _) = a
exprAnn (AFree a _ _) = a
exprAnn (AOr a _ _) = a
exprAnn (ACase a _ _ _) = a
exprAnn (ATyped a _ _) = a
exprAnn (ALet a _ _) = a

altAnn :: ABranchExpr a -> a
altAnn (ABranch _ e) = exprAnn e
