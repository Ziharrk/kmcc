module Curry.Annotate (annotateND) where

import Control.Arrow (second)
import qualified Data.Map as Map

import Curry.FlatCurry.Typeable (Typeable(..))
import Curry.FlatCurry.Typed.Type (TExpr (..), TBranchExpr (..), TPattern (..), TypeExpr)
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

annotateND :: NDAnalysisResult -> TExpr -> AExpr NDInfo
annotateND _ (TVarE _ x) = AVar NonDet x
annotateND _ (TLit _ l) = ALit Det l
annotateND analysis (TComb _ ct qname args) =
  AComb ann ct (qname, annHere) args'
  where
    annHere = Map.findWithDefault Det qname analysis
    args' = map (annotateND analysis) args
    ann = maximum (annHere : map exprAnn args')
annotateND analysis (TFree bs e) =
  AFree NonDet (map (second (const NonDet)) bs) (annotateND analysis e)
annotateND analysis (TOr e1 e2) =
  AOr NonDet (annotateND analysis e1) (annotateND analysis e2)
annotateND analysis (TCase ct e alts) =
  ACase ann ct e' alts'
  where
    ann = maximum (exprAnn e' : map altAnn alts')
    e' = annotateND analysis e
    alts' = map (annotateAlt analysis) alts
annotateND analysis (TTyped e t) =
  ATyped Det (annotateND analysis e) t
annotateND analysis (TLet bs e) =
  ALet ann bs' e'
  where
    e' = annotateND analysis e
    ann = maximum (exprAnn e' : map (snd . fst) bs')
    bs' = map annBind bs
    annBind ((x, _), e2) =
      let e2' = annotateND analysis e2
      in ((x, exprAnn e2'), e2')

annotateAlt :: NDAnalysisResult -> TBranchExpr -> ABranchExpr NDInfo
annotateAlt analysis (TBranch x e) = ABranch (annotatePat x) (annotateND analysis e)

annotatePat :: TPattern -> APattern NDInfo
annotatePat (TPattern _ qname args) = APattern Det (qname, Det) (map (second (const NonDet)) args)
annotatePat (TLPattern _ l) = ALPattern Det l

exprType :: TExpr -> TypeExpr
exprType = typeOf
