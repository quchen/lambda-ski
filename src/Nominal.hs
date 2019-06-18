{-# LANGUAGE OverloadedStrings #-}

module Nominal (
    Var(..),
    Expr(..),
    freeVars,
    parse,
    unsafeParse,
    prettyAnsi
) where



import           Control.Applicative
import           Data.Char
import           Data.Set                                  (Set)
import qualified Data.Set                                  as S
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Void
import           Text.Megaparsec                           (Parsec, (<?>))
import qualified Text.Megaparsec                           as P
import qualified Text.Megaparsec.Char                      as P



newtype Var = Var Text
    deriving (Eq, Ord, Show)

data Expr
    = EVar Var
    | EApp Expr Expr
    | EAbs Var Expr
    deriving (Eq, Ord)

freeVars :: Expr -> Set Var
freeVars (EVar x)   = S.singleton x
freeVars (EApp f x) = freeVars f <> freeVars x
freeVars (EAbs x e) = S.delete x (freeVars e)

prettyAnsi :: Expr -> Doc AnsiStyle
prettyAnsi = prettyPrec 0

prettyPrec :: Int -> Expr -> Doc AnsiStyle
prettyPrec _ (EVar var) = annotate (variableStyle var) (pretty var)
prettyPrec p (EApp e1 e2) = parenthesize (p > 10)
    (align (sep [prettyPrec 10 e1, prettyPrec (10+1) e2]))
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

data ParensNecessary = Parens | NoParens

instance Pretty Expr where
    pretty = go NoParens
      where
        go _ (EVar x) = pretty x
        go Parens x = parens (go NoParens x)
        go NoParens (EApp e1 e2)
          = let parentheses = case e1 of EAbs{} -> Parens; _ -> NoParens
            in align (sep [go parentheses e1, go Parens e2])
        go _ lamAbs@EAbs{}
          = let collectArgs args expr = case expr of
                    EAbs y e' -> collectArgs (y:args) e'
                    _other -> (args, expr)
                (collectedArgs, finalExpr) = collectArgs [] lamAbs
                finalArgs = map pretty (reverse collectedArgs)
            in "λ " <> hsep finalArgs <> dot <+> go NoParens finalExpr

instance Pretty Var where
    pretty (Var v) = pretty v

instance Show Expr where
    show = renderString . layoutPretty defaultLayoutOptions . pretty

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
