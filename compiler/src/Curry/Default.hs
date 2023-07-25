module Curry.Default where

import Control.Arrow (second)
import qualified Data.Set as Set

import Data.List ( isPrefixOf )

import Curry.FlatCurry (TypeExpr(..), VarIndex, QName)
import Curry.FlatCurry.Typed.Type (TExpr (..), TBranchExpr (..), TProg (..), TFuncDecl(..), TRule (..), TPattern (..))

defaultAmbiguous :: TProg -> TProg
defaultAmbiguous (TProg imps fixs typs decls ops) =
  TProg imps fixs typs (map defaultAmbiguousDecl decls) ops

defaultAmbiguousDecl :: TFuncDecl -> TFuncDecl
defaultAmbiguousDecl (TFunc nm arity vis ty rule) =
  TFunc nm arity vis ty (defaultAmbiguousRule (tyVars ty) rule)

-- collect forall-bound and free type variables
tyVars :: TypeExpr -> Set.Set VarIndex
tyVars (ForallType vs ty)     = tyVars ty `Set.union` Set.fromList (map fst vs)
tyVars (FuncType ty1 ty2)     = tyVars ty1 `Set.union` tyVars ty2
tyVars (TVar v)               = Set.singleton v
tyVars (TCons _ tys)          = Set.unions (map tyVars tys)

defaultAmbiguousRule :: Set.Set VarIndex -> TRule -> TRule
defaultAmbiguousRule vs (TRule lhs rhs) =
  TRule lhs (defaultAmbiguousExpr vs rhs)
defaultAmbiguousRule _ (TExternal ty str) =
  TExternal ty str

defaultAmbiguousExpr :: Set.Set VarIndex -> TExpr -> TExpr
defaultAmbiguousExpr vs (TVarE ty x)
  | tyVars ty `Set.isSubsetOf` vs = TVarE ty x
  | otherwise                     = TTyped (TVarE ty x) (defaultType vs ty)
defaultAmbiguousExpr _ (TLit ty l) = TLit ty l
defaultAmbiguousExpr vs (TComb ty ct qn args)
  -- Do not default under dictionaries, since they could hide forall'd vars that should remain free
  -- Defaulting will never be necessary there,
  -- since there are no function applications in a Dict binding that might lead to vanishing type variables.
  | "_Dict#" `isPrefixOf` snd qn = TComb ty ct qn args
  | tyvs `Set.isSubsetOf` vs     = TComb ty ct qn args'
  | otherwise                    = TTyped (TComb ty ct qn args') (defaultType vs ty)
  where
    tyvs = tyVars ty
    args' = map (defaultAmbiguousExpr (vs `Set.union` tyvs)) args
defaultAmbiguousExpr vs (TFree bs e) =
  TFree bs (defaultAmbiguousExpr vs e)
defaultAmbiguousExpr vs (TOr e1 e2) =
  TOr (defaultAmbiguousExpr vs e1) (defaultAmbiguousExpr vs e2)
defaultAmbiguousExpr vs (TCase ct e alts) =
  TCase ct (defaultAmbiguousExpr vs e) (map (defaultAmbiguousAlt vs) alts)
defaultAmbiguousExpr vs (TTyped e ty) =
  TTyped (defaultAmbiguousExpr (vs `Set.union` tyVars ty) e) ty
defaultAmbiguousExpr vs (TLet bs e) =
  TLet (defaultAmbiguousBinding vs bs) (defaultAmbiguousExpr vs e)

defaultType :: Set.Set VarIndex -> TypeExpr -> TypeExpr
defaultType vs (ForallType vs' ty) =
  ForallType vs' (defaultType (vs `Set.union` Set.fromList (map fst vs')) ty)
defaultType vs (FuncType ty1 ty2) =
  FuncType (defaultType vs ty1) (defaultType vs ty2)
defaultType vs (TVar v)
  | v `Set.member` vs = TVar v
  | otherwise         = TCons anyQName []
defaultType vs (TCons qn tys) = TCons qn (map (defaultType vs) tys)

defaultAmbiguousAlt :: Set.Set VarIndex -> TBranchExpr -> TBranchExpr
defaultAmbiguousAlt vs (TBranch x e) =
  TBranch (defaultAmbiguousPat vs x) (defaultAmbiguousExpr vs e)

defaultAmbiguousPat :: Set.Set VarIndex -> TPattern -> TPattern
defaultAmbiguousPat vs (TPattern ty qn args) =
  TPattern (defaultType vs ty) qn (map (second (defaultType vs)) args)
defaultAmbiguousPat _ p@(TLPattern _ _) = p

defaultAmbiguousBinding :: Set.Set VarIndex
                        -> [((VarIndex, TypeExpr), TExpr)]
                        -> [((VarIndex, TypeExpr), TExpr)]
defaultAmbiguousBinding vs = map (second (defaultAmbiguousExpr vs))

anyQName :: QName
anyQName = ("Special", "Any")
