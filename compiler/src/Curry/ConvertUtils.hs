
{-# LANGUAGE TypeFamilyDependencies #-}
module Curry.ConvertUtils where

import Language.Haskell.Exts hiding (Kind)

import Curry.Base.Ident (isInfixOp, mkIdent)
import Curry.FlatCurry (TVarWithKind, Kind (..))
import qualified Curry.FlatCurry as Curry
import Curry.FlatCurry.Annotated.Type (AExpr (..), VarIndex, ABranchExpr (..))
import Options (SearchStrat (..))

import Haskell.ExtsInstances ()

newtype UnqualName = Unqual Curry.QName

type instance HsEquivalent UnqualName = Name ()
type instance HsEquivalent Curry.QName = QName ()
type family HsEquivalent a = hs | hs -> a

indexToName :: Int -> Name ()
indexToName i = [Ident () (c : rest) | n <- [0 :: Int ..], let rest = if n == 0 then "" else show n, c <- ['a'..'z']] !! i

mkApplicativeChain :: Exp () -> [Exp ()] -> Exp ()
mkApplicativeChain e [] = mkReturn e
mkApplicativeChain e xs = InfixApp () e (QVarOp () dollarApplicativeQualName)
  (foldl1 (flip (InfixApp ()) (QVarOp () starApplicativeQualName)) xs)

mkFromHaskell :: Exp () -> Exp ()
mkFromHaskell = App () (Var () fromHaskellQualName)

mkFrom :: Exp () -> Exp ()
mkFrom = App () (Var () fromQualName)

mkFromHaskellTyped :: Exp () -> Type () -> Exp ()
mkFromHaskellTyped e = ExpTypeSig () (mkFromHaskell e) . mkCurry

mkToHaskell :: Exp () -> Exp ()
mkToHaskell = App () (Var () toHaskellQualName)

mkTo :: Exp () -> Exp ()
mkTo = App () (Var () toQualName)

toTyVarBndr :: TVarWithKind -> TyVarBind ()
toTyVarBndr (i, k) = KindedVar () (indexToName i) (kindToHsType k)

kindToHsType :: Kind -> Type ()
kindToHsType KStar = TyCon () (Qual () (ModuleName () "Data.Kind") (Ident () "Type"))
kindToHsType (KArrow k1 k2) = TyFun () (kindToHsType k1) (kindToHsType k2)

splitKindArrow :: Kind -> ([Kind], Kind)
splitKindArrow KStar = ([], KStar)
splitKindArrow (KArrow k1 k2) = (k1 : args, res)
  where
    (args, res) = splitKindArrow k2

mkShareableType :: Type () -> Type ()
mkShareableType = TyApp () (TyApp () (TyCon () shareableQualName) (TyCon () curryQualName))

mkCurryClassType :: Type () -> Type ()
mkCurryClassType = TyApp () (TyCon () curryClassQualName)

mkNFDataClassType :: Type () -> Type ()
mkNFDataClassType = TyApp () (TyCon () nfDataQualName)

mkRealNFDataClassType :: Type () -> Type ()
mkRealNFDataClassType = TyApp () (TyCon () realNFDataQualName)

mkLiftedFunc :: Type () -> Type () -> Type ()
mkLiftedFunc t1 = TyApp () (TyApp () (TyCon () liftedFuncQualName) t1)

mkCurry :: Type () -> Type ()
mkCurry = TyApp () (TyCon () curryQualName)

mkMonadicApp :: Exp () -> Exp () -> Exp ()
mkMonadicApp e1 = App () (App () (Var () appQualName) e1)

mkMplus :: Exp () -> Exp () -> Exp ()
mkMplus e1 = App () (App () (Var () mplusQualName) e1)

mkFailed :: Exp ()
mkFailed = Var () failedQualName

mkUnify :: Exp () -> Exp () -> Exp ()
mkUnify e = App () (App () (Var () unifyQualName) e)

mkUnifyWith :: Exp () -> Exp () -> Exp () -> Exp ()
mkUnifyWith e1 e2 = App () (App () (App () (Var () unifyWithQualName) e1) e2)

mkBindVar :: Exp () -> Exp () -> Exp ()
mkBindVar e = App () (App () (Var () bindVarQualName) e)

mkLiteralCase :: Exp () -> Exp () -> Exp () -> Exp ()
mkLiteralCase e1 e2 = App () (App () (App () (Var () literalCaseQualName) e1) e2)

data VarUse = None | One | Many
  deriving (Eq, Ord, Enum, Show)

instance Semigroup VarUse where
  None <> x = x
  x <> None = x
  _ <> _ = Many

instance Monoid VarUse where
  mempty = None

countVarUse :: AExpr a -> VarIndex -> VarUse
countVarUse (AVar _ idx) vidx = if idx == vidx then One else None
countVarUse (AComb _ _ _ args) vidx = mconcat (map (`countVarUse` vidx) args)
countVarUse (ALet _ bs e) vidx = case mconcat (map (\((_, _), b) -> countVarUse b vidx) bs) of
  None -> countVarUse e vidx
  _ -> Many
countVarUse (AFree _ _ e) vidx = countVarUse e vidx
countVarUse (AOr _ e1 e2) vidx = max (countVarUse e1 vidx) (countVarUse e2 vidx)
countVarUse (ACase _ _ e bs) vidx = maximum (map (\(ABranch _ e') -> scrUse <> countVarUse e' vidx) bs)
  where
    scrUse = countVarUse e vidx
countVarUse (ALit _ _) _ = None
countVarUse (ATyped _ e _) vidx = countVarUse e vidx

mkFromHaskellBind :: Int -> Type () -> Exp () -> Exp ()
mkFromHaskellBind i ty = Let () (BDecls ()
  [PatBind () (PVar () (appendName "_nd" (indexToName i)))
    (UnGuardedRhs () (ExpTypeSig () (mkFromHaskell (Var () (UnQual () (indexToName i)))) (mkCurry ty))) Nothing])

mkShareBind :: (Name (), Exp ()) -> Exp () -> Exp ()
mkShareBind (v, e1@(App _ (Var _ fromHs) _)) e2 | fromHs == fromHaskellQualName =
  mkLetBind (v, e1) e2
mkShareBind (v, e1) e2 = mkBind (mkShare e1) (Lambda () [PVar () v] e2)

mkLetBind :: (Name (), Exp ()) -> Exp () -> Exp ()
mkLetBind (v, e1) = Let () (BDecls () [PatBind () (PVar () v) (UnGuardedRhs () e1) Nothing])

mkShareLet :: Exp () -> [(Name (), Exp (), VarUse, Bool)] -> Exp ()
mkShareLet e [] = e
mkShareLet e bs
  | all (\(_, _, m, _) -> notMany m) bs = foldr go e bs
  | otherwise = mkBind (mkMFix fixLam) bindLam
  where
    go (a, b, _, True)  = mkLetBind (a, b)
    go (a, b, _, False) = mkShareBind (a, b)
    notMany Many = False
    notMany _    = True
    lamHead = Lambda () [mkLazyTuplePat $ map (\(v, _, _, _) -> PVar () v) bs]
    fixLam = lamHead $ mkApplicativeChain (mkTupleCon (length bs))
                     $ map (\(_, e',_, _) -> mkShare e') bs
    bindLam = lamHead e

mkShare :: Exp () -> Exp ()
mkShare = App () (Var () shareQualName)

mkFree :: Exp ()
mkFree = Var () freeQualName

mkNarrowSameConstr :: Exp () -> Exp ()
mkNarrowSameConstr = App () (Var () narrowSameConstrQualName)

mkReturn :: Exp () -> Exp ()
mkReturn = App () (Var () returnQualName)

mkFmap :: Exp () -> Exp ()
mkFmap = App () (Var () (Qual () (ModuleName () "P") (Ident () "fmap")))

mkFmapPartial :: Exp () -> Exp ()
mkFmapPartial = App () (Var () (Qual () (ModuleName () "B") (Ident () "mapCurryPartial")))

mkBind :: Exp () -> Exp () -> Exp ()
mkBind e1 = InfixApp () e1 (QConOp () bindQualName)

mkRight :: Exp () -> Exp ()
mkRight = App () (Var () rightQualName)

mkLeft :: Exp () -> Exp ()
mkLeft = App () (Var () leftQualName)

mkVal :: Exp () -> Exp ()
mkVal = App () (Var () valQualName)

mkEitherToCurry :: Exp () -> Exp ()
mkEitherToCurry = App () (Var () eitherToCurryQualName)

mkTupleCon :: Int -> Exp ()
mkTupleCon 0 = Var () (Special () (UnitCon ()))
mkTupleCon 1 = Var () (Qual () (ModuleName () "P") (Ident () "id"))
mkTupleCon n = Var () (Special () (TupleCon () Boxed n))

mkLazyTuplePat :: [Pat ()] -> Pat ()
mkLazyTuplePat [p] = PParen () p
mkLazyTuplePat xs  = PParen () $ PIrrPat () $ PTuple () Boxed xs

mkMFix :: Exp () -> Exp ()
mkMFix = App () (Var () (Qual () (ModuleName () "M") (Ident () "mfix")))

mkLazyUnify :: Exp () -> Exp () -> Exp ()
mkLazyUnify e = App () (App () (Var () lazyUnifyQualName) e)

mkAddToVarHeap :: Exp () -> Exp () -> Exp ()
mkAddToVarHeap e1 = App () (App () (Var () addToVarHeapQualName) e1)

mkCondSeq :: Exp () -> Exp () -> Exp ()
mkCondSeq e = App () (App () (Var () condSeqQualName) e)

mkShowStringCurry :: String -> Exp ()
mkShowStringCurry s = App () (Var () showStringCurryQualName) (Lit () (String () s s))

mkShowsCurryHighPrec :: Exp () -> Exp ()
mkShowsCurryHighPrec = App () (App () (Var () showsFreePrecCurryQualName) (Lit () (Int () 9 "9")))

mkShowsFreePrec :: Exp () -> Exp () -> Exp ()
mkShowsFreePrec e1 = App () (App () (Var () showsFreePrecQualName) e1)

mkAsTypeOf :: Exp () -> Exp () -> Exp ()
mkAsTypeOf e1 = App () (App () (Var () asTypeOfQualName) e1)

mkShowSpace :: Exp () -> Exp () -> Exp ()
mkShowSpace e1 = App () (App () (Var () showSpaceCurryQualName) e1)

mkShowsBrackets :: Exp () -> Exp () -> Exp ()
mkShowsBrackets e1 = App () (App () (Var () showsBracketsCurryQualName) e1)

mkGetVarId :: Exp () -> Exp ()
mkGetVarId = App () (Var () getVarIdQualName)

mkAddVarIds :: Exp () -> [Exp ()] -> Exp ()
mkAddVarIds e es = App () (App () (Var () addVarIdsQualName) e) (List () es)

showsBracketsCurryQualName :: QName ()
showsBracketsCurryQualName = Qual () (ModuleName () "B") (Ident () "showsBracketsCurry")

showSpaceCurryQualName :: QName ()
showSpaceCurryQualName = Qual () (ModuleName () "B") (Ident () "showSpaceCurry")

showsFreePrecCurryQualName :: QName ()
showsFreePrecCurryQualName = Qual () (ModuleName () "B") (Ident () "showsFreePrecCurry")

showsFreePrecQualName :: QName ()
showsFreePrecQualName = Qual () (ModuleName () "B") (Ident () "showsFreePrec")

asTypeOfQualName :: QName ()
asTypeOfQualName = Qual () (ModuleName () "P") (Ident () "asTypeOf")

getVarIdQualName :: QName ()
getVarIdQualName = Qual () (ModuleName () "B") (Ident () "getVarId")

addVarIdsQualName :: QName ()
addVarIdsQualName = Qual () (ModuleName () "B") (Ident () "addVarIds")

mplusQualName :: QName ()
mplusQualName = Qual () (ModuleName () "M") (Ident () "mplus")

shareableQualName :: QName ()
shareableQualName = Qual () (ModuleName () "B") (Ident () "Shareable")

unifiableQualName :: QName ()
unifiableQualName = Qual () (ModuleName () "B") (Ident () "Unifiable")

narrowableQualName :: QName ()
narrowableQualName = Qual () (ModuleName () "B") (Ident () "Narrowable")

narrowSameConstrQualName :: QName ()
narrowSameConstrQualName = Qual () (ModuleName () "B") (Ident () "narrowConstr")

liftedFuncQualName :: QName ()
liftedFuncQualName = Qual () (ModuleName () "B") (Ident () "LiftedFunc")

curryQualName :: QName ()
curryQualName = Qual () (ModuleName () "B") (Ident () "Curry")

dollarApplicativeQualName :: QName ()
dollarApplicativeQualName = Qual () (ModuleName () "P") (Symbol () "<$>")

starApplicativeQualName :: QName ()
starApplicativeQualName = Qual () (ModuleName () "P") (Symbol () "<*>")

fromHaskellQualName :: QName ()
fromHaskellQualName = Qual () (ModuleName () "B") (Ident () "fromHaskell")

fromQualName :: QName ()
fromQualName = Qual () (ModuleName () "B") (Ident () "from")

toHaskellQualName :: QName ()
toHaskellQualName = Qual () (ModuleName () "B") (Ident () "toHaskell")

toQualName :: QName ()
toQualName = Qual () (ModuleName () "B") (Ident () "to")

hsEquivQualName :: QName ()
hsEquivQualName = Qual () (ModuleName () "B") (Ident () "HsEquivalent")

nfDataQualName :: QName ()
nfDataQualName = Qual () (ModuleName () "B") (Ident () "NFDataC")

realNFDataQualName :: QName ()
realNFDataQualName = Qual () (ModuleName () "P") (Ident () "NFData")

seqQualName :: QName ()
seqQualName = Qual () (ModuleName () "P") (Ident () "seq")

rnfQualName :: QName ()
rnfQualName = Qual () (ModuleName () "B") (Ident () "rnfC")

hsToQualName :: QName ()
hsToQualName = Qual () (ModuleName () "B") (Ident () "ToHs")

hsFromQualName :: QName ()
hsFromQualName = Qual () (ModuleName () "B") (Ident () "FromHs")

appQualName :: QName ()
appQualName = Qual () (ModuleName () "B") (Ident () "app")

freeQualName :: QName ()
freeQualName = Qual () (ModuleName () "B") (Ident () "free")

failedQualName :: QName ()
failedQualName = Qual () (ModuleName () "M") (Ident () "mzero")

bindQualName :: QName ()
bindQualName = Qual () (ModuleName () "M") (Symbol () ">>=")

shareQualName :: QName ()
shareQualName = Qual () (ModuleName () "B") (Ident () "share")

curryClassQualName :: QName ()
curryClassQualName = Qual () (ModuleName () "B") (Ident () "Curryable")

normalFormQualName :: QName ()
normalFormQualName = Qual () (ModuleName () "B") (Ident () "NormalForm")

showFreeClassQualName :: QName ()
showFreeClassQualName = Qual () (ModuleName () "B") (Ident () "ShowFree")

showStringCurryQualName :: QName ()
showStringCurryQualName = Qual () (ModuleName () "B") (Ident () "showsStringCurry")

primitiveQualName :: QName ()
primitiveQualName = Qual () (ModuleName () "B") (Ident () "HasPrimitiveInfo")

returnQualName :: QName ()
returnQualName = Qual () (ModuleName () "M") (Ident () "return")

rightQualName :: QName ()
rightQualName = Qual () (ModuleName () "P") (Ident () "Right")

leftQualName :: QName ()
leftQualName = Qual () (ModuleName () "P") (Ident () "Left")

mkReturnFunc :: Exp () -> Exp ()
mkReturnFunc = App () (Var () returnFuncQualName)

returnFuncQualName :: QName ()
returnFuncQualName = Qual () (ModuleName () "B") (Ident () "returnFunc")

valQualName :: QName ()
valQualName = Qual () (ModuleName () "B") (Ident () "Val")

eitherToCurryQualName :: QName ()
eitherToCurryQualName = Qual () (ModuleName () "B") (Ident () "eitherToCurry")

unifyQualName :: QName ()
unifyQualName = Qual () (ModuleName () "B") (Ident () "unify")

unifyWithQualName :: QName ()
unifyWithQualName = Qual () (ModuleName () "B") (Ident () "unifyWith")

lazyUnifyQualName :: QName ()
lazyUnifyQualName = Qual () (ModuleName () "B") (Ident () "unifyL")

bindVarQualName :: QName ()
bindVarQualName = Qual () (ModuleName () "B") (Ident () "bindVar")

literalCaseQualName :: QName ()
literalCaseQualName = Qual () (ModuleName () "B") (Ident () "literalCase")

addToVarHeapQualName :: QName ()
addToVarHeapQualName = Qual () (ModuleName () "B") (Ident () "addToVarHeapM")

condSeqQualName :: QName ()
condSeqQualName = Qual () (ModuleName () "B") (Ident () "condSeq")

mainWrapperDetQualName :: QName ()
mainWrapperDetQualName = Qual () (ModuleName () "B") (Ident () "mainWrapperDet")

mainWrapperNDetQualName :: QName ()
mainWrapperNDetQualName = Qual () (ModuleName () "B") (Ident () "mainWrapperNDet")

exprWrapperDetQualName :: QName ()
exprWrapperDetQualName = Qual () (ModuleName () "B") (Ident () "exprWrapperDet")

exprWrapperNDetQualName :: QName ()
exprWrapperNDetQualName = Qual () (ModuleName () "B") (Ident () "exprWrapperNDet")

trueQualName :: QName ()
trueQualName = Qual () (ModuleName () "P") (Ident () "True")

falseQualName :: QName ()
falseQualName = Qual () (ModuleName () "P") (Ident () "False")

searchStratQualName :: SearchStrat -> QName ()
searchStratQualName DFS = Qual () (ModuleName () "B") (Ident () "dfs")
searchStratQualName BFS = Qual () (ModuleName () "B") (Ident () "bfs")
searchStratQualName FS  = Qual () (ModuleName () "B") (Ident () "fs")

anyHsQualName :: QName ()
anyHsQualName = Qual () (ModuleName () "B") (Ident () "Any")

appendName :: String -> Name () -> Name ()
appendName suff (Ident  () s) = Ident  () (s ++ suff)
appendName suff (Symbol () s) = Symbol () (s ++ suff)

isListOrTuple :: (String, String) -> Bool
isListOrTuple ("Prelude", "[]")   = True
isListOrTuple ("Prelude", xs)
  | Just _ <- tupleStringArity xs = True
isListOrTuple _                   = False

tupleStringArity :: String -> Maybe Int
tupleStringArity s = case s of
  '(':rest | last s == ')' -> Just $ length rest
  _                        -> Nothing

isOpQName :: (String , String) -> Bool
isOpQName (_, s) = isInfixOp (mkIdent s)
