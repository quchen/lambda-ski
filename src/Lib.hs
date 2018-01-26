{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Lib where



import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Coerce
import           Data.Foldable
import qualified Data.List            as L
import           Data.Semigroup
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Text.Megaparsec      ((<?>))
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P
import           Text.Megaparsec.Text (Parser)
import qualified Text.Megaparsec.Text as P



someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Var = Var Text deriving (Eq, Ord)
instance Show Var where show (Var x) = T.unpack x
instance IsString Var where fromString = (coerce :: Text -> Var) . fromString

data LExpr = LVar Var | LApp LExpr LExpr | LAbs Var LExpr deriving (Eq, Ord, Show)

-- prettyLambda :: LExpr -> String
-- prettyLambda = \case
--     LVar x   -> show x
--     LApp f x -> "(" ++ prettyLambda f ++ " " ++ prettyLambda x ++ ")"
--     LAbs x e -> "(λ" ++ show x ++ ". " ++ prettyLambda e ++ ")"

data ParensNecessary = Parens | NoParens

prettyLambda :: LExpr -> String
prettyLambda = go NoParens
  where
    go _ (LVar x) = show x
    go Parens x = "(" ++ go NoParens x ++ ")"
    go NoParens (LApp e1 e2)
      = let parenthesize = case e1 of LAbs{} -> Parens; _ -> NoParens
        in go parenthesize e1 ++ " " ++ go Parens e2
    go _ lamAbs@LAbs{}
      = let collectArgs args expr = case expr of
                LAbs y e' -> collectArgs (y:args) e'
                _other -> (args, expr)
            (collectedArgs, finalExpr) = collectArgs [] lamAbs
            finalArgs = map show (reverse collectedArgs)
        in "λ" ++ L.intercalate " " finalArgs ++ ". " ++ go NoParens finalExpr

free :: LExpr -> Set Var
free = \case
    LVar x   -> S.singleton x
    LApp f x -> free f <> free x
    LAbs x e -> S.delete x (free e)

isCombinator :: LExpr -> Bool
isCombinator = S.null . free

data SExpr = S | K | SApp SExpr SExpr deriving (Eq, Ord, Show)

data SLExpr = SLS | SLK | SLVar Var | SLApp SLExpr SLExpr | SLAbs Var SLExpr deriving (Eq, Ord, Show)

lambdaToSki :: LExpr -> SExpr
lambdaToSki = slToS . go . lToSl
  where
    lToSl :: LExpr -> SLExpr
    lToSl = \case
        LVar x   -> SLVar x
        LApp f x -> SLApp (lToSl f) (lToSl x)
        LAbs x e -> SLAbs x (lToSl e)

    slToS :: SLExpr -> SExpr
    slToS = \case
        SLS       -> S
        SLK       -> K
        e@SLVar{} -> error ("Variable " ++ show e ++ " unconverted")
        SLApp f x -> SApp (slToS f) (slToS x)
        e@SLAbs{} -> error ("Abstraction " ++ show e ++ " unconverted")

    go :: SLExpr -> SLExpr
    go = \case

        -- T[x] => x
        e@SLVar{}               -> e

        -- T[(E₁ E₂)] => (T[E₁] T[E₂])
        SLApp f x -> SLApp (go f) (go x)

        -- T[λx.x] => I
        SLAbs x (SLVar y) | x == y
            -> let i = SLApp (SLApp SLS SLK) SLK in i

        -- T[λx.E] => (K T[E]) (if x does not occur free in E)
        SLAbs x e | not (x `occursFreeIn` e)
            -> SLApp SLK (go e)

        -- T[λx.(E₁ E₂)] => (S T[λx.E₁] T[λx.E₂]) (if x occurs free in E₁ or E₂)
        SLAbs x (SLApp e1 e2) | x `occursFreeIn` e1 || x `occursFreeIn` e2
            -> SLApp (SLApp SLS (go (SLAbs x e1)))
                                (go (SLAbs x e2))

        -- T[λx.λy.E] => T[λx.T[λy.E]] (if x occurs free in E)
        SLAbs x (SLAbs y e) | x `occursFreeIn` e
            -> go (SLAbs x (go (SLAbs y e)))

        -- errors
        e@SLS                   -> e
        e@SLK                   -> e
        e@(SLAbs _ SLS)         -> e
        e@(SLAbs _ SLK)         -> e
        e@(SLAbs _ (SLVar _))   -> e
        e@(SLAbs _ (SLApp _ _)) -> e
        e@(SLAbs _ (SLAbs _ _)) -> e

    occursFreeIn :: Var -> SLExpr -> Bool
    occursFreeIn x e = S.member x (freeSL e)

    freeSL :: SLExpr -> Set Var
    freeSL = \case
        SLS       -> mempty
        SLK       -> mempty
        SLVar x   -> S.singleton x
        SLApp f x -> freeSL f <> freeSL x
        SLAbs x e -> S.delete x (freeSL e)

prettySki :: SExpr -> String
prettySki = go NoParens
  where
    go _ S = "S"
    go _ K = "K"
    go Parens app@SApp{} = "(" ++ go NoParens app ++ ")"
    go NoParens (SApp x y) = go NoParens x ++ " " ++ go Parens y

-- Make a lambda term enormous by converting it to SKI and back a couple of
-- times.
enormousize :: Int -> LExpr -> LExpr
enormousize n = foldr (.) id (replicate n (skiToLambda . lambdaToSki))

skiToLambda :: SExpr -> LExpr
skiToLambda = \case
    S        -> lAbs ["f","g","x"]
                     (lApp (LVar "f") [LVar "x", LApp (LVar "g") (LVar "x")])
    K        -> lAbs ["x","y"] (LVar "x")
    SApp f x -> LApp (skiToLambda f) (skiToLambda x)

lApp :: LExpr -> [LExpr] -> LExpr
lApp = foldl LApp

lAbs :: [Var] -> LExpr -> LExpr
lAbs xs e = foldr LAbs e xs

-- Y combinator
-- λf. ((λx. (f (x x))) (λx. (f (x x))))
ycL :: LExpr
ycL = LAbs "f" (LApp (LAbs "x" (LApp (LVar "f")
                                     (LApp (LVar "x")
                                           (LVar "x"))))
                     (LAbs "x" (LApp (LVar "f")
                                     (LApp (LVar "x")
                                           (LVar "x")))))

-- Y combinator
ycS :: SExpr
ycS = S `SApp` S `SApp` K `SApp` (S `SApp` (K `SApp` (S `SApp` S `SApp` (S `SApp` (S `SApp` S `SApp` K)))) `SApp` K)

-- λx. x x
omegaL :: LExpr
omegaL = LAbs "x" (LApp (LVar "x") (LVar "x"))

-- (λx. x x) (λx. x x)
oMegaL :: LExpr
oMegaL = LApp omegaL omegaL

-- λx. x
idL :: LExpr
idL = LAbs "x" (LVar "x")

test :: LExpr
test = LAbs "x" (LAbs "y" (LVar "x"))

-- should be (S (K (S ((S K) K))) (S (K K) ((S K) K)))
test2 :: LExpr
test2 = LAbs "x" (LAbs "y" (LApp (LVar "y") (LVar "x")))

testConversion :: LExpr -> IO ()
testConversion term = do
    putStrLn ("Convert " ++ prettyLambda term)
    putStr "Result: "
    (putStrLn . prettySki . lambdaToSki) term

parseLambda :: Text -> LExpr
parseLambda input = case P.parse lExprP ("λ expression" :: String) input of
    Left err -> error (show err)
    Right r -> r

lExprP :: Parser LExpr
lExprP = lAbsP <|> lVarAppP
  where
    lAbsP, lAppP, lVarP :: Parser LExpr
    lAbsP = do
        _lambda <- tok (P.oneOf ("λ\\" :: [Char])) <?> "lambda"
        vars    <- P.someTill varP (tok (P.char '.'))
        body    <- lExprP
        pure (lAbs vars body)
    lAppP = do
        e1 <- lExprP
        e2 <- lExprP
        pure (LApp e1 e2)
    lVarP = fmap LVar varP
    varP = fmap (Var . T.pack) (tok (P.some variableCharP))
      where
        asciiChars = take 128 [minBound..]
        variableCharP = P.oneOf ("-_" ++ filter (\c -> isAlphaNum c) asciiChars)



-- lExprP :: Parser LExpr
-- lExprP = lVarP <|> funcAppP <|> lambdaAbsP
--   where
--     funcAppP = liftA2 LApp funcP argP
--
--     funcP = lVarP <|> parenthesized lambdaAbsP <|> funcAppP
--
--     argP = lVarP <|> parenthesized lambdaAbsP <|> parenthesized funcAppP
--
--     lambdaAbsP = do
--         _lambda <- tok (P.char 'λ' <|> P.char '\\')
--         vars    <- P.someTill varP (tok (P.char '.'))
--         body    <- lExprP
--         pure (lAbs vars body)
--
--     lVarP = fmap LVar varP
--     varP = fmap (Var . T.pack) (tok (P.some variableCharP))
--       where
--         asciiChars = take 128 [minBound..]
--         variableCharP = P.oneOf ("-_" ++ filter (\c -> isAlphaNum c) asciiChars)




     -- <expr>   ::=  <var>
     --             | <func> <arg>
     --             | lambda <var> . <expr>
     -- <func>   ::=  <var>
     --             | (lambda <var> . <expr>)
     --             | <func> <arg>
     -- <arg>    ::=  <var>
     --             | (lambda <var> . <expr>)
     --             | (<func> <arg>)
     -- <var>    ::= a| b| .... | Z

tok :: Parser a -> Parser a
tok p = p <* P.space

parenthesized :: Parser a -> Parser a
parenthesized = P.between (tok (P.char '(')) (tok (P.char ')'))
