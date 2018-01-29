{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where



import           Control.Applicative
import           Data.Char
import           Data.Coerce
import           Data.Map                                (Map)
import qualified Data.Map                                as M
import           Data.Semigroup
import           Data.Set                                (Set)
import qualified Data.Set                                as S
import           Data.String
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String
import           Text.Megaparsec                         ((<?>))
import qualified Text.Megaparsec                         as P
import           Text.Megaparsec.Text                    (Parser)



newtype Var = Var Text deriving (Eq, Ord)
instance Show Var where show (Var x) = T.unpack x
instance Pretty Var where pretty (Var x) = pretty x
instance IsString Var where fromString = (coerce :: Text -> Var) . fromString

data LExpr = LVar Var | LApp LExpr LExpr | LAbs Var LExpr deriving (Eq, Ord)

-- prettyLambda :: LExpr -> String
-- prettyLambda = \case
--     LVar x   -> show x
--     LApp f x -> "(" ++ prettyLambda f ++ " " ++ prettyLambda x ++ ")"
--     LAbs x e -> "(λ" ++ show x ++ ". " ++ prettyLambda e ++ ")"

data ParensNecessary = Parens | NoParens

instance Pretty LExpr where
    pretty = go NoParens
      where
        go _ (LVar x) = pretty x
        go Parens x = parens (go NoParens x)
        go NoParens (LApp e1 e2)
          = let parenthesize = case e1 of LAbs{} -> Parens; _ -> NoParens
            in align (sep [go parenthesize e1, go Parens e2])
        go _ lamAbs@LAbs{}
          = let collectArgs args expr = case expr of
                    LAbs y e' -> collectArgs (y:args) e'
                    _other -> (args, expr)
                (collectedArgs, finalExpr) = collectArgs [] lamAbs
                finalArgs = map pretty (reverse collectedArgs)
            in "λ" <> hsep finalArgs <> dot <+> go NoParens finalExpr

instance Show LExpr where
    show = renderString . layoutPretty defaultLayoutOptions . pretty

free :: LExpr -> Set Var
free = \case
    LVar x   -> S.singleton x
    LApp f x -> free f <> free x
    LAbs x e -> S.delete x (free e)

isCombinator :: LExpr -> Bool
isCombinator = S.null . free

data SExpr = S | K | SApp SExpr SExpr deriving (Eq, Ord)

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

instance Pretty SExpr where
    pretty = go NoParens
      where
        go _ S = "S"
        go _ K = "K"
        go Parens app@SApp{} = parens (go NoParens app)
        go NoParens (SApp x y) = align (sep [go NoParens x, go Parens y])

instance Show SExpr where
    show = renderString . layoutPretty defaultLayoutOptions . pretty

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
lApp f [] = f
lApp f (x:xs) = lApp (LApp f x) xs

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

-- should be S (K (S (S K K))) (S (K K) (S K K))
test2 :: LExpr
test2 = LAbs "x" (LAbs "y" (LApp (LVar "y") (LVar "x")))

parseLambda :: Text -> Either String LExpr
parseLambda input = case P.parse (P.space *> lExprP <* P.eof) ("λ expression" :: String) input of
    Left err -> Left (P.parseErrorPretty err)
    Right r -> Right r

unsafeParseLambda :: Text -> LExpr
unsafeParseLambda input = case parseLambda input of
    Left err -> error ("unsafeParseLambda parse error: " ++ err)
    Right r -> r

lExprP :: Parser LExpr
lExprP = do
    stuff <- P.some term
    case stuff of
        []     -> error "some is broken booo"
        [x]    -> pure x
        s:tuff -> pure (lApp s tuff)
  where
    term = varP <|> absP <|> parenthesized lExprP
    absP = do
        _lambda <- tok (P.oneOf ("λ\\" :: [Char])) <?> "lambda"
        vars    <- P.someTill plainVarP (tok (P.char '.'))
        body    <- lExprP
        pure (lAbs vars body)
    varP = fmap LVar plainVarP
    plainVarP = fmap (Var . T.pack) (tok (P.some variableCharP))
      where
        variableCharP = P.satisfy (\c -> (isAlphaNum c || isSymbol c || c `elem` ("_'<>+&-*/[]{}" :: String)) && c `notElem` ("λ\\." :: String))

tok :: Parser a -> Parser a
tok p = p <* P.space

parenthesized :: Parser a -> Parser a
parenthesized = P.between (tok (P.char '(')) (tok (P.char ')'))

parseLambdaTest :: Text -> IO ()
parseLambdaTest input = do
    putStrLn ("Parse: " ++ T.unpack input)
    putStrLn ""
    case parseLambda input of
        Left err -> do
            putStrLn "ERROR"
            putStrLn err
        Right r -> do
            print r

parseSki :: Text -> Either String SExpr
parseSki input = case P.parse (sExprP <* P.eof) ("SK expression" :: String) input of
    Left err -> Left (P.parseErrorPretty err)
    Right r -> Right r

sExprP :: Parser SExpr
sExprP = do
    stuff <- P.some term
    case stuff of
        []     -> error "some is broken booo"
        [x]    -> pure x
        s:tuff -> pure (sApp s tuff)
  where
    sP = tok (P.oneOf ("sS" :: String)) *> pure S
    kP = tok (P.oneOf ("kK" :: String)) *> pure K
    term = sP <|> kP <|> parenthesized sExprP

    sApp :: SExpr -> [SExpr] -> SExpr
    sApp f [] = f
    sApp f (x:xs) = sApp (SApp f x) xs

parseSkiTest :: Text -> IO ()
parseSkiTest input = do
    putStrLn ("Parse: " ++ T.unpack input)
    putStrLn ""
    case parseSki input of
        Left err -> do
            putStrLn "ERROR"
            putStrLn err
        Right r -> do
            print r

evalLambda :: LExpr -> LExpr
evalLambda = go M.empty
  where
    go :: Map Var LExpr -> LExpr -> LExpr
    go env lVar@(LVar var) = case M.lookup var env of
        Just replacement -> replacement
        Nothing          -> lVar

    go env (LAbs x e) = LAbs x (go (M.delete x env) e)

    go env (LApp e1 e2)
      = let e2' = go env e2
        in case go env e1 of
            LAbs x e1' -> go (M.insert x e2' env) e1'
            var@LVar{} -> LApp var e2'
            app@LApp{} -> LApp app e2'

-- Broken :-(
factorialLambda :: LExpr
factorialLambda = unsafeParseLambda "\
    \ (λpred mul true false Y.                    \
    \     (λisZero.                               \
    \         Y (λrec n. (isZero n)               \
    \                    n                        \
    \                    (mul n (rec (pred n)))   \
    \           )                                 \
    \     )                                       \
    \     (λn. n (λ_. false) true)                \
    \ )                                           \
    \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) \
    \ (λm n f x. m (n f) x)                       \
    \ (λt _. t)                                   \
    \ (λ_ f. f)                                   \
    \ (λf. (λx. f (x x)) (λx. f (x x)))           \
    \ (λf x. f (f (f x)))                         \
    \"

-- Broken :-(
fiboLambda :: LExpr
fiboLambda = unsafeParseLambda
    " (λsucc pred true false Y 1 2 isZero add sub leq. \
    \     Y (λrec n. (leq n 1)                         \
    \                n                                 \
    \                (add (rec (sub n 1))              \
    \                         (rec (sub n 2)))))       \
    \ (λn f x. f (n f x))                              \
    \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u))      \
    \ (λt _. t)                                        \
    \ (λ_ f. f)                                        \
    \ (λf. (λx. f (x x)) (λx. f (x x)))                \
    \ (λf x. f x)                                      \
    \ (λf x. f (f x))                                  \
    \ (λn. n (λ_. false) true)                         \
    \ (λm n. m succ n)                                 \
    \ (λm n. n pred m)                                 \
    \ (λm n. isZero (sub m n))                         "
