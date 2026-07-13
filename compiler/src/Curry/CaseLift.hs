module Curry.CaseLift (simplifyCaseExpressions) where

import Control.Arrow (first)
import Control.Monad.State ( State, get, put, runState )
import Data.IntMap (IntMap)
import Data.List.Extra (nubOrd)
import qualified Data.IntMap as IntMap

import Curry.FlatCurry.Typed.Type ( TProg(..), TypeExpr (..), TFuncDecl(..), TRule (..)
                                  , TExpr(..), TBranchExpr(..), TPattern(..), Visibility(..)
                                  , CombType(..), Kind(..), CaseType, VarIndex, QName, typeOf )
import Curry.FlatCurry.Typed.Goodies ( updTProgTFuncs, allVarsInTypeExpr )

-- |Reduces the size of case expressions in a typed FlatCurry program by
-- lifting each case expressions into separate functions.
-- This is done to avoid code explosion when generating code from the FlatCurry representation,
-- since the determinism optimization can lead to quadratic growth in the size/nesting of case expressions.
simplifyCaseExpressions :: TProg -> TProg
simplifyCaseExpressions = updTProgTFuncs simplify
  where
    simplify fs = concatMap simplifyFunc fs
    simplifyFunc (TFunc qn arity vis ty body) =
      let kinds = case ty of
            ForallType ks _ -> IntMap.fromList ks
            _               -> IntMap.empty
          (body', newFuncs) = simplifyCaseRule qn kinds body
      in TFunc qn arity vis ty body' : newFuncs

simplifyCaseRule :: QName -> IntMap Kind -> TRule -> (TRule, [TFuncDecl])
simplifyCaseRule qn kinds (TRule args e) = (TRule args e', funcs)
  where
    (e', (_,_, funcs, _)) = runState (findTopCase e) (0, qn, [], kinds)
simplifyCaseRule _ _ r@(TExternal _ _) = (r, [])

type SimpCaseM = State (Int, QName, [TFuncDecl], IntMap Kind)

mkNewFuncName :: SimpCaseM QName
mkNewFuncName = do
  (i, (mdl, qn), funcs, kinds) <- get
  let newName = qn ++ "_caseLifted_" ++ show i
  put (i + 1, (mdl, qn), funcs, kinds)
  return (mdl, newName)

addFunc :: TFuncDecl -> SimpCaseM ()
addFunc f = do
  (i, qn, funcs, kinds) <- get
  put (i, qn, f : funcs, kinds)

type VarSet = IntMap TypeExpr

-- do not split off the first case if possible
findTopCase :: TExpr -> SimpCaseM TExpr
findTopCase e@(TVarE _ _) = return e
findTopCase e@(TLit _ _) = return e
findTopCase e@(TComb _ _ _ _) =
  fst <$> simplifyCaseExpr e
findTopCase (TCase ct ty brs) = do
  brs' <- mapM ((fst <$>) . simplifyBranch) brs
  return (TCase ct ty brs')
findTopCase e@(TLet _ _) =
  fst <$> simplifyCaseExpr e
findTopCase (TFree binds body) = do
  body' <- fst <$> simplifyCaseExpr body
  return (TFree binds body')
findTopCase e@(TOr _ _) =
  fst <$> simplifyCaseExpr e
findTopCase (TTyped body ty) = do
  body' <- fst <$> simplifyCaseExpr body
  return (TTyped body' ty)

simplifyCaseExpr :: TExpr -> SimpCaseM (TExpr, VarSet)
simplifyCaseExpr e@(TVarE ty i) = return (e, IntMap.singleton i ty)
simplifyCaseExpr e@(TLit _ _) = return (e, IntMap.empty)
simplifyCaseExpr (TComb ty ct qn args) = do
  (args', vs) <- unzip <$> mapM simplifyCaseExpr args
  return (TComb ty ct qn args', IntMap.unions vs)
simplifyCaseExpr (TLet binds body) = do
  (binds', used, defined) <- simplifyBinds binds
  (body', vs') <- simplifyCaseExpr body
  return (TLet binds' body', (vs' `IntMap.union` used) `IntMap.difference` defined)
simplifyCaseExpr (TFree binds body) = do
  (body', vs') <- simplifyCaseExpr body
  return (TFree binds body', vs' `IntMap.difference` IntMap.fromList binds)
simplifyCaseExpr (TOr e1 e2) = do
  (e1', vs1) <- simplifyCaseExpr e1
  (e2', vs2) <- simplifyCaseExpr e2
  return (TOr e1' e2', vs1 `IntMap.union` vs2)
simplifyCaseExpr e@(TCase ct e' branches) =
  simplifyCase (typeOf e) ct e' branches
simplifyCaseExpr (TTyped e ty) = do
  (e', vs) <- simplifyCaseExpr e
  return (TTyped e' ty, vs)

simplifyBinds :: [((VarIndex, TypeExpr), TExpr)]
              -> SimpCaseM ([((VarIndex, TypeExpr), TExpr)], VarSet, VarSet)
simplifyBinds binds = do
  (binds', used) <- unzip <$> mapM (\(v, e) -> first (v,) <$> simplifyCaseExpr e) binds
  return (binds', IntMap.unions used, IntMap.fromList (map fst binds))

simplifyCase :: TypeExpr -> CaseType -> TExpr -> [TBranchExpr] -> SimpCaseM (TExpr, VarSet)
simplifyCase ty ct e branches = do
  (e', vs) <- simplifyCaseExpr e
  (branches', vs') <- unzip <$> mapM simplifyBranch branches
  (_, _, _, kinds) <- get
  let allVs = IntMap.unions (vs : vs')
  let vsList = IntMap.toList allVs
  newName <- mkNewFuncName
  let newFunc = TFunc newName (IntMap.size allVs) Private (ForallType typeVars newType) newRule
      newType = foldr (FuncType . snd) ty vsList
      -- TODO: The KStar assumption here is not correct for all cases.
      -- We should infer the actual kinds of the type variables in the expression.
      typeVars = map (\i -> (i,) $ IntMap.findWithDefault KStar i kinds) $
                  nubOrd $ allVarsInTypeExpr newType
      newRule = TRule vsList (TCase ct e' branches')
  addFunc newFunc
  let newExpr = TComb ty FuncCall newName (map (uncurry (flip TVarE)) vsList)
  return (newExpr, allVs)

simplifyBranch :: TBranchExpr -> SimpCaseM (TBranchExpr, VarSet)
simplifyBranch (TBranch pat e) = do
  let patVars = case pat of
        TPattern _ _ vars -> IntMap.fromList vars
        TLPattern _ _     -> IntMap.empty
  (e', vs) <- simplifyCaseExpr e
  return (TBranch pat e', vs `IntMap.difference` patVars)
