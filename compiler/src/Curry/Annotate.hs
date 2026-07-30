module Curry.Annotate (annotateND, annotateND') where

import Control.Arrow (second)
import qualified Data.Map as Map
import Data.Set (Set)

import Curry.FlatCurry.Typeable (Typeable(..))
import Curry.FlatCurry.Typed.Type (TExpr (..), TBranchExpr (..), TPattern (..), TypeExpr (..), QName)
import Curry.FlatCurry.Annotated.Type (AExpr(..), ABranchExpr (..), APattern(..))

import Curry.Analysis (NDInfo(..), NDAnalysisResult)
import Curry.ConvertUtils (exprAnn, altAnn)

annotateND :: NDAnalysisResult -> Set QName -> TExpr -> AExpr (TypeExpr, NDInfo)
annotateND analysis = annotateND' analysis Map.empty

annotateND' :: NDAnalysisResult -> Map.Map Int NDInfo -> Set QName -> TExpr -> AExpr (TypeExpr, NDInfo)
annotateND' _ vMap _ (TVarE ty x) = AVar (ty, Map.findWithDefault NonDet x vMap) x
annotateND' _ _ _ (TLit ty l) = ALit (ty, Det) l
annotateND' analysis vMap dataNames (TComb ty ct qname args) =
  AComb (ty, ann) ct (qname, (ty', annHere)) args'
  where
    argTys = map typeOf args
    ty' = foldr FuncType ty argTys
    annHere = Map.findWithDefault Det qname analysis
    args' = map (annotateND' analysis vMap dataNames) args
    ann = maximum (annHere : map (snd . exprAnn) args')
annotateND' analysis vMap dataNames (TFree bs e) =
  AFree (typeOf e, NonDet) (map (second (,NonDet)) bs)
        (annotateND' analysis vMap' dataNames e)
  where vMap' = Map.union vMap (Map.fromList (map ((,NonDet) . fst) bs))
annotateND' analysis vMap dataNames (TOr e1 e2) =
  AOr (typeOf e1, NonDet) (annotateND' analysis vMap dataNames e1)
                          (annotateND' analysis vMap dataNames e2)
annotateND' analysis vMap dataNames ex@(TCase ct e alts) =
  ACase (typeOf ex, ann) ct e' alts'
  where
    annScr = snd $ exprAnn e'
    ann = maximum (annScr : map (snd . altAnn) alts')
    e' = annotateND' analysis vMap dataNames e
    alts' = map (annotateAlt analysis vMap annScr dataNames) alts
annotateND' analysis vMap dataNames (TTyped e ty) =
  let e' = annotateND' analysis vMap dataNames e
  in  ATyped (exprAnn e') e' ty
annotateND' analysis vMap dataNames (TLet bs e) =
  ALet (typeOf e, ann) bs' e'
  where
    e' = annotateND' analysis vMap' dataNames e
    vMap' = Map.union vMap (Map.fromList (map (\((i,(_,nd)),_) -> (i, nd)) bs'))
    ann = maximum (snd (exprAnn e') : map (snd . snd . fst) bs')
    bs' = map annBind bs
    annBind ((x, _), e2) =
      let e2' = annotateND' analysis vMap dataNames e2
      in ((x, exprAnn e2'), e2')

annotateAlt :: NDAnalysisResult -> Map.Map Int NDInfo -> NDInfo
            -> Set QName -> TBranchExpr
            -> ABranchExpr (TypeExpr, NDInfo)
annotateAlt analysis vMap ann dataNames (TBranch x e) = normalBranch
  where
    normalBranch = ABranch (annotatePat x) (annotateND' analysis vMapNormal dataNames e)
    vMapNormal = Map.union vMap (Map.fromList (map ((,ann) . fst) (patArgs x)))
    patArgs (TPattern _ _ args) = args
    patArgs (TLPattern _ _)     = []

annotatePat :: TPattern -> APattern (TypeExpr, NDInfo)
annotatePat (TPattern ty qname args) =
  APattern (ty, Det) (qname, (ty, Det)) (map (second (,NonDet)) args)
annotatePat (TLPattern ty l) = ALPattern (ty, Det) l
