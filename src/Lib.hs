module Lib where

import           Data.Semigroup
import           Data.Set       (Set)
import qualified Data.Set       as S

someFunc :: IO ()
someFunc = putStrLn "someFunc"


newtype Var = Var String deriving (Eq, Ord, Show)
data LExpr = LVar Var | LApp LExpr LExpr | LAbs Var LExpr deriving (Eq, Ord, Show)

free :: LExpr -> Set Var
free (LVar x) = S.singleton x
free (LApp f x) = free f <> free x
free (LAbs x e) = S.delete x (free e)

data SExpr = S | K | SApp SExpr SExpr deriving (Eq, Ord, Show)

data SLExpr = SLS | SLK | SLVar Var | SLApp SLExpr SLExpr | SLAbs Var SLExpr deriving (Eq, Ord, Show)

lambdaToSki :: LExpr -> SExpr
lambdaToSki = slToS . go . lToSl
  where
    lToSl :: LExpr -> SLExpr
    lToSl (LVar x) = SLVar x
    lToSl (LApp x1 x2) = SLApp (lToSl x1) (lToSl x2)
    lToSl (LAbs x1 x2) = SLAbs x1 (lToSl x2)

    slToS :: SLExpr -> SExpr
    slToS SLS = S
    slToS SLK = K
    slToS e@(SLVar _) = error ("Variable " ++ show e ++ " unconverted")
    slToS (SLApp x1 x2) = SApp (slToS x1) (slToS x2)
    slToS e@(SLAbs _ _) = error ("Abstraction " ++ show e ++ " unconverted")


    go :: SLExpr -> SLExpr
    go e@SLS = e
    go e@SLK = e
    go e@(SLVar _) = e
    go (SLApp f x) = SLApp (go f) (go x)
    go e@(SLAbs _ SLS) = e
    go e@(SLAbs _ SLK) = e
    go (SLAbs x (SLVar y))
        | x == y = let i = SLApp (SLApp SLS SLK) SLK in i
    go (SLAbs x e)
        | not (x `occursFreeIn` e) = SLApp SLK (go e)
    go (SLAbs x (SLApp e1 e2))
        | x `occursFreeIn` e1 || x `occursFreeIn` e2
            = SLApp (SLApp SLS (go (SLAbs x e1))) (go (SLAbs x e2))
    go (SLAbs x (SLAbs y e))
        | x `occursFreeIn` e = go (SLAbs x (go (SLAbs y e)))
    go e = e

    -- T[x] => x
    -- T[(E₁ E₂)] => (T[E₁] T[E₂])
    -- T[λx.E] => (K T[E]) (if x does not occur free in E)
    -- T[λx.x] => I
    -- T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E)
    -- T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)

    occursFreeIn :: Var -> SLExpr -> Bool
    occursFreeIn x e = S.member x (freeSL e)

    freeSL :: SLExpr -> Set Var
    freeSL SLS = mempty
    freeSL SLK = mempty
    freeSL (SLVar x) = S.singleton x
    freeSL (SLApp f x) = freeSL f <> freeSL x
    freeSL (SLAbs x e) = S.delete x (freeSL e)

prettySki :: SExpr -> String
prettySki S = "S"
prettySki K = "K"
prettySki (SApp (SApp x1 x2) x3) = prettySki x1 ++ " " ++ prettySki x2 ++ " " ++ prettySki x3
prettySki (SApp x1 x2) = "(" ++ prettySki x1 ++ " " ++ prettySki x2 ++ ")"

yc :: LExpr
yc = LAbs (Var "f") (LApp (LAbs (Var "x") (LApp (LVar (Var "f")) (LApp (LVar (Var "x")) (LVar (Var "x"))))) (LAbs (Var "x") (LApp (LVar (Var "f")) (LApp (LVar (Var "x")) (LVar (Var "x"))))))
