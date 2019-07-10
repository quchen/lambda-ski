{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Convert (
    nominalToDeBruijn,
    deBruijnToNominal,

    nominalToSki,
    skiToNominal
) where



import           Control.Applicative
import qualified Data.Map.Strict     as M
import           Data.Set            (Set)
import qualified Data.Set            as S
import qualified Data.Text           as T
import           Numeric.Natural

import DeBruijn as B
import Nominal  as N
import Ski      as S



nominalToDeBruijn :: N.Expr -> B.Expr
nominalToDeBruijn = go mempty
  where
    go :: M.Map Var Natural -> N.Expr -> B.Expr
    go !levels (N.EVar var@(Var name)) = case M.lookup var levels of
        Just level -> B.EVar level name
        Nothing    -> B.EVarFree name
    go levels (N.EApp f x) = B.EApp (go levels f) (go levels x)
    go levels (N.EAbs var body) = B.EAbs (go (M.insert var 0 (M.map (+1) levels)) body)

deBruijnToNominal :: B.Expr -> N.Expr
deBruijnToNominal = go 0
  where
    go :: Natural -> B.Expr -> N.Expr
    go _n (B.EVar _ix name) = N.EVar (Var name)
    go _n (B.EVarFree name) = N.EVar (Var name)
    go n (B.EApp f x) = N.EApp (go n f) (go n x)
    go n (B.EAbs body) = N.EAbs
        (case findBinderName n body of
            Just var -> var
            Nothing -> Var (T.singleton '_'))
        (go n body)

findBinderName :: Natural -> B.Expr -> Maybe Var
findBinderName n (B.EVar n' name)
    | n == n'   = Just (Var name)
    | otherwise = Nothing
findBinderName _ B.EVarFree{} = Nothing
findBinderName n (B.EApp f x) = findBinderName n f <|> findBinderName n x
findBinderName n (B.EAbs body) = findBinderName (n+1) body

data SkiLambda
    = SLS                       -- ^ S
    | SLK                       -- ^ K
    | SLI                       -- ^ I
    | SLB                       -- ^ B
    | SLC                       -- ^ C
    | SLVar N.Var               -- ^ x
    | SLApp SkiLambda SkiLambda -- ^ e1 e2
    | SLAbs N.Var SkiLambda     -- ^ λx. e
    deriving (Eq, Ord, Show)

nominalToSki :: Bool -> N.Expr -> S.Expr
nominalToSki allowICB = irToSki . translate . nominalToIr
  where
    nominalToIr :: N.Expr -> SkiLambda
    nominalToIr = \case
        N.EVar x   -> SLVar x
        N.EApp f x -> SLApp (nominalToIr f) (nominalToIr x)
        N.EAbs x e -> SLAbs x (nominalToIr e)

    irToSki :: SkiLambda -> S.Expr
    irToSki = \case
        SLS              -> S
        SLK              -> K
        SLI              -> I
        SLB              -> B
        SLC              -> C
        SLVar (Var name) -> S.EFree name
        SLApp f x        -> S.EApp (irToSki f) (irToSki x)
        e@SLAbs{}        -> error ("Abstraction " <> show e <> " unconverted")

    translate :: SkiLambda -> SkiLambda
    translate = \case

        -- 1. T[x] => x
        var@SLVar{} -> var

        -- 2. T[(E₁ E₂)] => (T[E₁] T[E₂])
        SLApp f x -> SLApp (translate f) (translate x)

        -- 3. T[λx.E] => (K T[E]) (if x does not occur free in E)
        SLAbs x e | not (x `occursFreeIn` e)
            -> SLApp SLK (translate e)

        -- 4. T[λx.x] => I
        SLAbs x (SLVar y) | x == y
            -> if allowICB then SLI else SLApp (SLApp SLS SLK) SLK

        -- 5. T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E)
        SLAbs x (SLAbs y e) | x `occursFreeIn` e
            -> translate (SLAbs x (translate (SLAbs y e)))

        SLAbs x (SLApp e1 e2)

            -- 6. T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in both E₁ and E₂)
            | (allowICB && x `occursFreeIn` e1 && x `occursFreeIn` e2) || not allowICB
                -> SLApp (SLApp SLS (translate (SLAbs x e1)))
                                    (translate (SLAbs x e2))

            -- 7. T[λx.(E₁ E₂)] => (C T[λx.E₁] T[E₂]) (if x occurs free in E₁ but not E₂)
            | x `occursFreeIn` e1 && not (x `occursFreeIn` e2)
                -> SLApp (SLApp SLC (translate (SLAbs x e1)))
                                    (translate e2)

            -- 8. T[λx.(E₁ E₂)] => (B T[E₁] T[λx.E₂]) (if x occurs free in E₂ but not E₁)
            | not (x `occursFreeIn` e1) && x `occursFreeIn` e2
                -> SLApp (SLApp SLB (translate e1))
                                    (translate (SLAbs x e2))

        -- errors
        e@SLS                   -> e
        e@SLK                   -> e
        e@SLI                   -> e
        e@SLB                   -> e
        e@SLC                   -> e
        e@(SLAbs _ SLS)         -> e
        e@(SLAbs _ SLK)         -> e
        e@(SLAbs _ SLI)         -> e
        e@(SLAbs _ SLB)         -> e
        e@(SLAbs _ SLC)         -> e
        e@(SLAbs _ (SLVar _))   -> e
        e@(SLAbs _ (SLApp _ _)) -> e
        e@(SLAbs _ (SLAbs _ _)) -> e

    occursFreeIn :: N.Var -> SkiLambda -> Bool
    occursFreeIn x e = S.member x (freeSL e)

    freeSL :: SkiLambda -> Set N.Var
    freeSL = \case
        SLS       -> mempty
        SLK       -> mempty
        SLI       -> mempty
        SLB       -> mempty
        SLC       -> mempty
        SLVar x   -> S.singleton x
        SLApp f x -> freeSL f <> freeSL x
        SLAbs x e -> S.delete x (freeSL e)

skiToNominal :: S.Expr -> N.Expr
skiToNominal = \case
    S            -> "λf g x. f x (g x)"
    K            -> "λx _. x"
    I            -> "λx. x"
    B            -> "λf g x. f (g x)"
    C            -> "λf y x. f x y"
    S.EFree name -> N.EVar (N.Var name)
    S.EApp f x   -> N.EApp (skiToNominal f) (skiToNominal x)
