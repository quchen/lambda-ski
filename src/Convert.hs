{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Convert (
    nominalToDeBruijn,
    unsafeNominalToDeBruijn,
    deBruijnToNominal
) where



import           Control.Applicative
import           Data.List
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import           Numeric.Natural

import DeBruijn as B
import Nominal  as N



nominalToDeBruijn :: N.Expr -> Either (S.Set N.Var) B.Expr
nominalToDeBruijn e = case freeVars e of
    free | not (S.null free) -> Left free
    _closed -> Right (n2b mempty e)

unsafeNominalToDeBruijn :: N.Expr -> B.Expr
unsafeNominalToDeBruijn e = case nominalToDeBruijn e of
    Left free      -> error ("Cannot convert nominal to De Bruijn because of free variables: " ++ intercalate ", " [T.unpack name | Var name <- S.elems free])
    Right deBruijn -> deBruijn

n2b :: M.Map Var Natural -> N.Expr -> B.Expr
n2b !levels (N.EVar var@(Var name)) = B.EVar (levels M.! var) name
n2b levels (N.EApp f x) = B.EApp (n2b levels f) (n2b levels x)
n2b levels (N.EAbs var body) = B.EAbs (n2b (M.insert var 0 (M.map (+1) levels)) body)

deBruijnToNominal :: B.Expr -> N.Expr
deBruijnToNominal = db2n 0

db2n :: Natural -> B.Expr -> N.Expr
db2n _n (B.EVar _ix name) = N.EVar (Var name)
db2n n (B.EApp f x) = N.EApp (db2n n f) (db2n n x)
db2n n (B.EAbs body) = N.EAbs
    (case findBinderName n body of
        Just var -> var
        Nothing -> Var (T.singleton '_'))
    (db2n n body)

findBinderName :: Natural -> B.Expr -> Maybe Var
findBinderName n (B.EVar n' name)
    | n == n'   = Just (Var name)
    | otherwise = Nothing
findBinderName n (B.EApp f x) = findBinderName n f <|> findBinderName n x
findBinderName n (B.EAbs body) = findBinderName (n+1) body
