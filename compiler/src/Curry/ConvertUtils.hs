
{-# LANGUAGE TypeFamilyDependencies #-}
module Curry.ConvertUtils where

import Language.Haskell.Exts hiding (Kind)

import Curry.Base.Ident (isInfixOp, mkIdent)
import Curry.FlatCurry (TVarWithKind, Kind (..))
import qualified Curry.FlatCurry as Curry
import Curry.FlatCurry.Annotated.Type (AExpr (..), VarIndex, ABranchExpr (..))

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

mkToHaskell :: Exp () -> Exp ()
mkToHaskell = App () (Var () toHaskellQualName)

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

mkFromHaskellBind :: Int -> Exp () -> Exp ()
mkFromHaskellBind i = Let () (BDecls ()
  [PatBind () (PVar () (appendName "_nd" (indexToName i)))
    (UnGuardedRhs () (mkFromHaskell (Var () (UnQual () (indexToName i))))) Nothing])

mkShareBind :: (Name (), Exp (), VarUse) -> Exp () -> Exp ()
mkShareBind (_, _, None) e2 = e2
mkShareBind (v, e1, One ) e2 = Let () (BDecls () [PatBind () (PVar () v) (UnGuardedRhs () e1) Nothing]) e2
mkShareBind (v, e1, Many) e2 = mkBind (mkShare e1) (Lambda () [PVar () v] e2)

mkShare :: Exp () -> Exp ()
mkShare = App () (Var () shareQualName)

mkFree :: Exp ()
mkFree = Var () freeQualName

mkNarrowSameConstr :: Exp () -> Exp ()
mkNarrowSameConstr = App () (Var () narrowSameConstrQualName)

mkReturn :: Exp () -> Exp ()
mkReturn = App () (Var () returnQualName)

mkBind :: Exp () -> Exp () -> Exp ()
mkBind e1 = InfixApp () e1 (QConOp () bindQualName)

mkRight :: Exp () -> Exp ()
mkRight = App () (Var () rightQualName)

mkLeft :: Exp () -> Exp ()
mkLeft = App () (Var () leftQualName)

mkValUnshared :: Exp () -> Exp ()
mkValUnshared = App () (App () (Var () valQualName) (Var () unsharedQualName))

mkEitherToCurry :: Exp () -> Exp ()
mkEitherToCurry = App () (Var () eitherToCurryQualName)

mkShareLet :: Exp () -> [(Name (), Exp (), VarUse)] -> Exp ()
mkShareLet e [] = e
mkShareLet e bs
  | all (\(_, _, m) -> notMany m) bs = foldr mkShareBind e bs
  | otherwise = mkBind (mkMFix fixLam) bindLam
  where
    notMany Many = False
    notMany _    = True
    lamHead = Lambda () [mkLazyTuplePat $ map (\(v, _, _) -> PVar () v) bs]
    fixLam = lamHead $ mkApplicativeChain (mkTupleCon (length bs))
                     $ map (\(_, e',_) -> mkShare e') bs
    bindLam = lamHead e

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
mkAddToVarHeap e = App () (App () (Var () addToVarHeapQualName) e)

mkShowStringCurry :: String -> Exp ()
mkShowStringCurry s = App () (Var () showStringCurryQualName) (Lit () (String () s s))

mkShowsCurryHighPrec :: Exp () -> Exp ()
mkShowsCurryHighPrec = App () (App () (Var () showsFreePrecCurryQualName) (Lit () (Int () 9 "9")))

mkShowSpace :: Exp () -> Exp () -> Exp ()
mkShowSpace e1 = App () (App () (Var () showSpaceCurryQualName) e1)

mkShowsBrackets :: Exp () -> Exp () -> Exp ()
mkShowsBrackets e1 = App () (App () (Var () showsBracketsCurryQualName) e1)

mkGetVarId :: Exp () -> Exp ()
mkGetVarId = App () (Var () getVarIdQualName)

mkAddVarIds :: Exp () -> [Exp ()] -> Exp ()
mkAddVarIds e es = App () (App () (Var () addVarIdsQualName) e) (List () es)

showsBracketsCurryQualName :: QName ()
showsBracketsCurryQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "showsBracketsCurry")

showSpaceCurryQualName :: QName ()
showSpaceCurryQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "showSpaceCurry")

showsFreePrecCurryQualName :: QName ()
showsFreePrecCurryQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "showsFreePrecCurry")

getVarIdQualName :: QName ()
getVarIdQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "getVarId")

addVarIdsQualName :: QName ()
addVarIdsQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "addVarIds")

mplusQualName :: QName ()
mplusQualName = Qual () (ModuleName () "M") (Ident () "mplus")

shareableQualName :: QName ()
shareableQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "Shareable")

unifiableQualName :: QName ()
unifiableQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "Unifiable")

narrowableQualName :: QName ()
narrowableQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "Narrowable")

narrowSameConstrQualName :: QName ()
narrowSameConstrQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "narrowConstr")

liftedFuncQualName :: QName ()
liftedFuncQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "LiftedFunc")

curryQualName :: QName ()
curryQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "Curry")

dollarApplicativeQualName :: QName ()
dollarApplicativeQualName = Qual () (ModuleName () "P") (Symbol () "<$>")

starApplicativeQualName :: QName ()
starApplicativeQualName = Qual () (ModuleName () "P") (Symbol () "<*>")

fromHaskellQualName :: QName ()
fromHaskellQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "fromHaskell")

toHaskellQualName :: QName ()
toHaskellQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "toHaskell")

hsEquivQualName :: QName ()
hsEquivQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "HsEquivalent")

hsToQualName :: QName ()
hsToQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "ToHs")

hsFromQualName :: QName ()
hsFromQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "FromHs")

appQualName :: QName ()
appQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "app")

freeQualName :: QName ()
freeQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "free")

failedQualName :: QName ()
failedQualName = Qual () (ModuleName () "M") (Ident () "mzero")

bindQualName :: QName ()
bindQualName = Qual () (ModuleName () "M") (Symbol () ">>=")

shareQualName :: QName ()
shareQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "share")

curryClassQualName :: QName ()
curryClassQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "Curryable")

normalFormQualName :: QName ()
normalFormQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "NormalForm")

showFreeClassQualName :: QName ()
showFreeClassQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "ShowFree")

showStringCurryQualName :: QName ()
showStringCurryQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "showsStringCurry")

primitiveQualName :: QName ()
primitiveQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "HasPrimitiveInfo")

returnQualName :: QName ()
returnQualName = Qual () (ModuleName () "M") (Ident () "return")

rightQualName :: QName ()
rightQualName = Qual () (ModuleName () "P") (Ident () "Right")

leftQualName :: QName ()
leftQualName = Qual () (ModuleName () "P") (Ident () "Left")

mkReturnFunc :: Exp () -> Exp ()
mkReturnFunc = App () (Var () returnFuncQualName)

returnFuncQualName :: QName ()
returnFuncQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "returnFunc")

valQualName :: QName ()
valQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "Val")

unsharedQualName :: QName ()
unsharedQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "Unshared")

eitherToCurryQualName :: QName ()
eitherToCurryQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "eitherToCurry")

unifyQualName :: QName ()
unifyQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "unify")

unifyWithQualName :: QName ()
unifyWithQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "unifyWith")

lazyUnifyQualName :: QName ()
lazyUnifyQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "unifyL")

addToVarHeapQualName :: QName ()
addToVarHeapQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "addToVarHeapM")

mainWrapperDetQualName :: QName ()
mainWrapperDetQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "mainWrapperDet")

mainWrapperNDetQualName :: QName ()
mainWrapperNDetQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "mainWrapperNDet")

exprWrapperDetQualName :: QName ()
exprWrapperDetQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "exprWrapperDet")

exprWrapperNDetQualName :: QName ()
exprWrapperNDetQualName = Qual () (ModuleName () "BasicDefinitions") (Ident () "exprWrapperNDet")

trueQualName :: QName ()
trueQualName = Qual () (ModuleName () "P") (Ident () "True")

falseQualName :: QName ()
falseQualName = Qual () (ModuleName () "P") (Ident () "False")

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
