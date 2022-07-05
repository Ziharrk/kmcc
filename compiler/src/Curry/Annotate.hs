module Curry.Annotate (annotateND, isFunFree) where

import Control.Arrow (second)
import qualified Data.Map as Map

import Curry.FlatCurry.Typeable (Typeable(..))
import Curry.FlatCurry.Typed.Type (TExpr (..), TBranchExpr (..), TPattern (..), TypeExpr (..))
import Curry.FlatCurry.Annotated.Type (AExpr(..), ABranchExpr (..), APattern(..))

import Curry.Analysis (NDInfo(..), NDAnalysisResult)

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

annotateND :: NDAnalysisResult -> TExpr -> AExpr (TypeExpr, NDInfo)
annotateND _ (TVarE ty x) = AVar (ty, NonDet) x
annotateND _ (TLit ty l) = ALit (ty, Det) l
annotateND analysis (TComb ty ct qname args) =
  AComb (ty, ann) ct (qname, (ty, annHere)) args'
  where
    annHere = case Map.lookup qname analysis of
                Nothing -> if all isFunFree (funArgs ty) then Det else NonDet
                Just det -> det
    args' = map (annotateND analysis) args
    ann = maximum (annHere : map (snd . exprAnn) args')
annotateND analysis (TFree bs e) =
  AFree (typeOf e, NonDet) (map (second (,NonDet)) bs) (annotateND analysis e)
annotateND analysis (TOr e1 e2) =
  AOr (typeOf e1, NonDet) (annotateND analysis e1) (annotateND analysis e2)
annotateND analysis ex@(TCase ct e alts) =
  ACase (typeOf ex, ann) ct e' alts'
  where
    ann = maximum (snd (exprAnn e') : map (snd . altAnn) alts')
    e' = annotateND analysis e
    alts' = map (annotateAlt analysis) alts
annotateND analysis (TTyped e ty) =
  ATyped (ty, Det) (annotateND analysis e) ty
annotateND analysis (TLet bs e) =
  ALet (typeOf e, ann) bs' e'
  where
    e' = annotateND analysis e
    ann = maximum (snd (exprAnn e') : map (snd . snd . fst) bs')
    bs' = map annBind bs
    annBind ((x, _), e2) =
      let e2' = annotateND analysis e2
      in ((x, exprAnn e2'), e2')

annotateAlt :: NDAnalysisResult -> TBranchExpr -> ABranchExpr (TypeExpr, NDInfo)
annotateAlt analysis (TBranch x e) = ABranch (annotatePat x) (annotateND analysis e)

annotatePat :: TPattern -> APattern (TypeExpr, NDInfo)
annotatePat (TPattern ty qname args) = APattern (ty, Det) (qname, (ty, Det)) (map (second (,NonDet)) args)
annotatePat (TLPattern ty l) = ALPattern (ty, Det) l

funArgs :: TypeExpr ->  [TypeExpr]
funArgs (FuncType t1 t2) = t1 : funArgs t2
funArgs _ = []

isFunFree :: TypeExpr -> Bool
isFunFree (FuncType _ _) = False
isFunFree (TVar _) = False
isFunFree (ForallType _ ty) = isFunFree ty
isFunFree (TCons _ args) = all isFunFree args
