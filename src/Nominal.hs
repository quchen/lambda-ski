{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Nominal (
    Var(..),
    Expr(..),
    define,
    parse,
    unsafeParse,
    prettyAnsi
) where



import           Control.Applicative
import           Data.Char
import           Data.String
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Void
import           Text.Megaparsec                           (Parsec, (<?>))
import qualified Text.Megaparsec                           as P
import qualified Text.Megaparsec.Char                      as P



newtype Var = Var Text
    deriving (Eq, Ord, Show)

instance IsString Var where
    fromString = Var . T.pack

data Expr
    = EVar Var
    | EApp Expr Expr
    | EAbs Var Expr
    deriving (Eq, Ord)

instance IsString Expr where
    fromString = unsafeParse . T.pack

instance Lift Expr where
    lift (EVar var) = [| EVar $(lift var) |]
    lift (EApp f x) = [| EApp $(lift f)  $(lift x) |]
    lift (EAbs var body) = [| EAbs $(lift var) $(lift body) |]

instance Lift Var where
    lift (Var name) = [| Var (T.pack $(lift (T.unpack name))) |]

prettyAnsi :: Expr -> Doc AnsiStyle
prettyAnsi = prettyPrec 0

prettyPrec :: Int -> Expr -> Doc AnsiStyle
prettyPrec _ (EVar var) = annotate (variableStyle var) (pretty var)
prettyPrec p (EApp e1 e2)
  = let collectArgs (EApp e e') arg = let (hd, args) = collectArgs e e'
                                      in (hd, args ++ [arg])
        collectArgs hd arg = (hd, [arg])
        (f, xyz) = collectArgs e1 e2
    in parenthesize (p > 10) (prettyPrec 10 f <+> align (vsep (map (prettyPrec (10+1)) xyz)))
prettyPrec p lamAbs@EAbs{}
  = let collectArgs args expr = case expr of
             EAbs y e' -> collectArgs (y:args) e'
             _other -> (args, expr)
        (collectedArgs, finalExpr) = collectArgs [] lamAbs
        finalArgs = map (\var -> annotate (variableStyle var) (pretty var)) (reverse collectedArgs)
     in parenthesize (p > 5) ("λ " <> hsep finalArgs <> dot <+> prettyPrec 5 finalExpr)

variableStyle :: Var -> AnsiStyle
variableStyle (Var name)
    | Just ('_',_) <- T.uncons name = color Black
    | otherwise = case T.foldl (\acc c -> mod (acc + ord c) 6) 0 name of
        0 -> color Red
        1 -> color Green
        2 -> color Yellow
        3 -> color Blue
        4 -> color Magenta
        5 -> color Cyan
        _ -> error "Cannot happen because of modulus"

parenthesize :: Bool -> Doc ann -> Doc ann
parenthesize p x
    | p = parens x
    | otherwise = x

instance Pretty Expr where
    pretty = unAnnotate . prettyAnsi

instance Pretty Var where
    pretty (Var v) = pretty v

instance Show Expr where
    showsPrec _ (EVar (Var name)) = showString (T.unpack name)
    showsPrec p (EApp e1 e2) = showParen (p > 10)
        (showsPrec 10 e1 . showChar ' ' . showsPrec (10+1) e2)
    showsPrec p (EAbs (Var name) e) = showParen (p > 5) (showChar 'λ' . showString (T.unpack name ++ ". ") . showsPrec 5 e)

-- | Replace all occurrences of a free 'Var'iable with some 'Expr'ession. Useful
-- to include programs in others.
--
-- @
-- 'define' ('Var' "BODY") program stdlib
-- @
define
    :: (Var, Expr) -- ^ Variable and replacement
    -> Expr -- ^ Program in which to do the replacement
    -> Expr
define (v, val) eVar@(EVar v')
    | v == v' = val
    | otherwise = eVar
define dfn (EApp f x) = EApp (define dfn f) (define dfn x)
define dfn@(v, _) eAbs@(EAbs v' e)
    | v == v' = eAbs
    | otherwise = EAbs v' (define dfn e)

type Parser = Parsec Void Text

parse :: Text -> Either String Expr
parse input = case P.parse (P.space *> eExprP <* P.eof) ("λ expression" :: String) input of
    Left err -> Left (P.errorBundlePretty err)
    Right r -> Right r

unsafeParse :: Text -> Expr
unsafeParse input = case parse input of
    Left err -> error ("unsafeParse parse error: " ++ err)
    Right r -> r

eExprP :: Parser Expr
eExprP = do
    stuff <- P.some termP
    case stuff of
        []     -> error "some is broken booo"
        [x]    -> pure x
        s:tuff -> pure (lApp s tuff)
  where
    termP :: Parser Expr
    termP = eVarBodyP <|> eAbsP <|> parenthesized eExprP

    eAbsP :: Parser Expr
    eAbsP = do
        _lambda <- tok (P.oneOf ("λ\\" :: [Char])) <?> "lambda"
        vars    <- P.someTill varBinderP (tok (P.char '.'))
        body    <- eExprP
        pure (lAbs vars body)

    eVarBodyP :: Parser Expr
    eVarBodyP = fmap EVar varP

    validVariableChar :: Char -> Bool
    validVariableChar c = not (isSpace c || elem c ("()λ\\." :: String))

    varBinderP :: Parser Var
    varBinderP = fmap (Var . T.pack) (tok (P.some (P.satisfy validVariableChar)))

    varP :: Parser Var
    varP = do
        var@(Var name) <- varBinderP
        case T.head name of
            '_' -> fail "Variables starting with »_« may only be used as (unused) binders"
            _other -> pure var

    lApp :: Expr -> [Expr] -> Expr
    lApp = foldl EApp

    lAbs :: [Var] -> Expr -> Expr
    lAbs xs e = foldr EAbs e xs

tok :: Parser a -> Parser a
tok p = p <* P.space

parenthesized :: Parser a -> Parser a
parenthesized = P.between (tok (P.char '(')) (tok (P.char ')'))
