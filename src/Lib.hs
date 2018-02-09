{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where



import           Control.Applicative
import           Control.Monad.State
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

data LExpr var = LVar var | LApp (LExpr var) (LExpr var) | LAbs var (LExpr var) deriving (Eq, Ord)

-- prettyLambda :: LExpr -> String
-- prettyLambda = \case
--     LVar x   -> show x
--     LApp f x -> "(" ++ prettyLambda f ++ " " ++ prettyLambda x ++ ")"
--     LAbs x e -> "(λ" ++ show x ++ ". " ++ prettyLambda e ++ ")"

data ParensNecessary = Parens | NoParens

instance Pretty var => Pretty (LExpr var) where
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

instance Pretty var => Show (LExpr var) where
    show = renderString . layoutPretty defaultLayoutOptions . pretty

free :: Ord var => LExpr var -> Set var
free = \case
    LVar x   -> S.singleton x
    LApp f x -> free f <> free x
    LAbs x e -> S.delete x (free e)

isCombinator :: Ord var => LExpr var -> Bool
isCombinator = S.null . free

data SExpr
    = S
    | K
    | SApp SExpr SExpr
    deriving (Eq, Ord)

data SLExpr var
    = SLS                             -- ^ S
    | SLK                             -- ^ K
    | SLVar var                       -- ^ x
    | SLApp (SLExpr var) (SLExpr var) -- ^ e1 e2
    | SLAbs var (SLExpr var)          -- ^ λx. e
    deriving (Eq, Ord, Show)

data Validation e a = Error e | Success a deriving (Eq, Ord, Show)
instance Functor (Validation e) where
    fmap _ (Error e) = Error e
    fmap f (Success a) = Success (f a)
instance Semigroup e => Applicative (Validation e) where
    pure = Success
    Error e1  <*> Error e2  = Error (e1 <> e2)
    Error e   <*> Success _ = Error e
    Success _ <*> Error e   = Error e
    Success f <*> Success x = Success (f x)

unsafeFromSuccess :: Validation e a -> a
unsafeFromSuccess (Success x) = x
unsafeFromSuccess (Error _) = error "unsafeFromSuccess (Error e)"

lambdaToSki :: (Ord var, Show var) => LExpr var -> Validation [Text] SExpr
lambdaToSki = slToS . go . lToSl
  where
    lToSl :: LExpr var -> SLExpr var
    lToSl = \case
        LVar x   -> SLVar x
        LApp f x -> SLApp (lToSl f) (lToSl x)
        LAbs x e -> SLAbs x (lToSl e)

    slToS :: Show var => SLExpr var -> Validation [Text] SExpr
    slToS = \case
        SLS       -> pure S
        SLK       -> pure K
        e@SLVar{} -> Error ["Variable " <> T.pack (show e) <> " unconverted"]
        SLApp f x -> liftA2 SApp (slToS f) (slToS x)
        e@SLAbs{} -> Error ["Abstraction " <> T.pack (show e) <> " unconverted"]

    go :: Ord var => SLExpr var -> SLExpr var
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

    occursFreeIn :: Ord var => var -> SLExpr var -> Bool
    occursFreeIn x e = S.member x (freeSL e)

    freeSL :: Ord var => SLExpr var -> Set var
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
unsafeEnormousize :: Int -> LExpr Var -> LExpr Var
unsafeEnormousize n = foldr (.) id (replicate n (skiToLambda . unsafeFromSuccess . lambdaToSki))

skiToLambda :: SExpr -> LExpr Var
skiToLambda = \case
    S        -> lAbs ["f","g","x"]
                     (lApp (LVar "f") [LVar "x", LApp (LVar "g") (LVar "x")])
    K        -> lAbs ["x","y"] (LVar "x")
    SApp f x -> LApp (skiToLambda f) (skiToLambda x)

lApp :: LExpr var -> [LExpr var] -> LExpr var
lApp f [] = f
lApp f (x:xs) = lApp (LApp f x) xs

lAbs :: [var] -> LExpr var -> LExpr var
lAbs xs e = foldr LAbs e xs

-- Y combinator
-- λf. ((λx. (f (x x))) (λx. (f (x x))))
ycL :: LExpr Var
ycL = LAbs "f" (LApp (LAbs "x" (LApp (LVar "f")
                                     (LApp (LVar "x")
                                           (LVar "x"))))
                     (LAbs "x" (LApp (LVar "f")
                                     (LApp (LVar "x")
                                           (LVar "x")))))

parseLambda :: Text -> Either String (LExpr Var)
parseLambda input = case P.parse (P.space *> lExprP <* P.eof) ("λ expression" :: String) input of
    Left err -> Left (P.parseErrorPretty err)
    Right r -> Right r

unsafeParseLambda :: Text -> LExpr Var
unsafeParseLambda input = case parseLambda input of
    Left err -> error ("unsafeParseLambda parse error: " ++ err)
    Right r -> r

lExprP :: Parser (LExpr Var)
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

alphaRename :: Eq var => var -> var -> LExpr var -> LExpr var
alphaRename before after (LVar var)
    | var == before = LVar after
    | otherwise = LVar var
alphaRename before after (LAbs x e)
    | x == before = LAbs after (alphaRename before after e)
    | otherwise = LAbs x (alphaRename before after e)
alphaRename before after (LApp e1 e2)
    = LApp (alphaRename before after e1) (alphaRename before after e2)

evalLambda :: LExpr Var -> LExpr Var
evalLambda = unRename . go M.empty . rename
  where
    go :: Map (Var,Integer) (LExpr (Var,Integer)) -> LExpr (Var,Integer) -> LExpr (Var,Integer)
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

-- Give all variables unique names, so that capture-avoiding substitution is
-- unnecessary.
rename :: Ord var => LExpr var -> LExpr (var, Integer)
rename = flip evalState M.empty . enumerate
  where
    enumerate :: Ord var => LExpr var -> State (Map var Integer) (LExpr (var, Integer))
    enumerate (LVar x) = do
        multiplicity <- gets (M.lookup x)
        case multiplicity of
            Just index -> pure (LVar (x, index))
            Nothing -> modify (M.insert x 0) >> pure (LVar (x, 0))
    enumerate (LAbs x e) = do
        multiplicity <- gets (M.lookup x)
        let xIndex = case multiplicity of
                Just index -> index+1
                Nothing -> 0
        modify (M.insert x xIndex)
        LAbs (x, xIndex) <$> enumerate e
    enumerate (LApp e1 e2) = liftA2 LApp (enumerate e1) (enumerate e2)

unRename :: LExpr (Var, Integer) -> LExpr Var
-- unRename (LVar (Var var, 0)) = LVar (Var var)
unRename (LVar (Var var, i)) = LVar (Var (var <> T.pack (show i)))
unRename (LAbs (var, _) e)   = LAbs var (unRename e)
unRename (LApp e1 e2)        = LApp (unRename e1) (unRename e2)

-- Broken :-(
factorialLambda :: LExpr Var
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
fiboLambda :: LExpr Var
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
