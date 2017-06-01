-----------------------------------------------------------------------------
--- A few base functions for analysing type dependencies in FlatCurry programs.
---
--- @author Heiko Hoffmann, Michael Hanus
--- @version Junes 2017
-----------------------------------------------------------------------------

module CASS.FlatCurryDependency(dependsDirectlyOnTypes,callsDirectly) where

import FlatCurry.Types
import List
import SetRBT

--- Return the type constructors occurring in a type declaration.
dependsDirectlyOnTypes :: TypeDecl -> [QName]

dependsDirectlyOnTypes (Type _ _ _ consDeclList) =
  nub (concatMap (\ (Cons _ _ _ typeExprs) -> concatMap tconsOf typeExprs)
                 consDeclList)

dependsDirectlyOnTypes (TypeSyn _ _ _ typeExpr) = nub (tconsOf typeExpr)


tconsOf :: TypeExpr -> [(String,String)]
tconsOf (TVar _) = []
tconsOf (FuncType a b) =  tconsOf a ++ tconsOf b
tconsOf (TCons qName texps) = qName : concatMap tconsOf texps


-----------------------------------------------------------------------------
-- list of direct dependencies for a function
callsDirectly :: FuncDecl -> [QName]
callsDirectly fun = setRBT2list (snd (directlyDependent fun))

-- set of direct dependencies for a function
directlyDependent :: FuncDecl -> (QName,SetRBT QName)
directlyDependent (Func f _ _ _ (Rule _ e))   = (f,funcSetOfExpr e)
directlyDependent (Func f _ _ _ (External _)) = (f,emptySet)

-- Gets the set of all functions (including partially applied functions)
-- called in an expression:
funcSetOfExpr :: Expr -> SetRBT QName
funcSetOfExpr (Var _) = emptySet
funcSetOfExpr (Lit _) = emptySet
funcSetOfExpr (Comb ct f es) =
  if isConstructorComb ct then unionMap funcSetOfExpr es
                          else insertRBT f (unionMap funcSetOfExpr es)
funcSetOfExpr (Free _ e) = funcSetOfExpr e
funcSetOfExpr (Let bs e) = unionRBT (unionMap (funcSetOfExpr . snd) bs)
                                    (funcSetOfExpr e)
funcSetOfExpr (Or e1 e2) = unionRBT (funcSetOfExpr e1) (funcSetOfExpr e2)
funcSetOfExpr (Case _ e bs) = unionRBT (funcSetOfExpr e)
                                       (unionMap funcSetOfBranch bs)
 where funcSetOfBranch (Branch _ be) = funcSetOfExpr be
funcSetOfExpr (Typed e _) = funcSetOfExpr e

isConstructorComb :: CombType -> Bool
isConstructorComb ct = case ct of
  ConsCall       -> True
  ConsPartCall _ -> True
  _              -> False

unionMap :: (a -> SetRBT QName) -> [a] -> SetRBT QName
unionMap f = foldr unionRBT emptySet . map f

emptySet :: SetRBT QName
emptySet = emptySetRBT leqQName

leqQName :: QName -> QName -> Bool
leqQName (m1,n1) (m2,n2) = m1++('.':n1) <= m2++('.':n2)
