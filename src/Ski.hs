{-# LANGUAGE OverloadedStrings #-}

module Ski (
    Expr(..),
    prettyAnsi,
    normalForm,
    removeAuxiliarySymbols,
    parse,
    unsafeParse
) where



import           Data.Char
import           Data.Foldable
import           Data.Functor
import           Data.Text                                 (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Void
import           Text.Megaparsec                           (Parsec)
import qualified Text.Megaparsec                           as P
import qualified Text.Megaparsec.Char                      as P

import Orphans ()



data Expr
    = S -- S x y z = x z (y z)

    | K -- »const«
        -- K x y = x

    | I -- »id«
        -- I = S K K
        -- I x = x

    | B -- »compose«
        -- B = S (K S) K
        -- B f g x = f (g x)

    | C -- »flip«
        -- C = S (S (K (S (K S) K)) S) (K K)
        -- C f y x = f x y

    | EApp Expr Expr
    deriving (Eq, Ord)

instance Show Expr where
    show = collapse . renderString . layoutPretty defaultLayoutOptions . pretty
      where
        collapse ('\n':rest) = collapse (' ':rest)
        collapse (x:y:rest)
            | isSpace x && isSpace y = collapse (y:rest)
        collapse (x:rest) = x : collapse rest
        collapse "" = ""

data ParensNecessary = Parens | NoParens

instance Pretty Expr where
    pretty = unAnnotate . prettyAnsi

prettyAnsi :: Expr -> Doc AnsiStyle
prettyAnsi = go NoParens (cycle [Red, Green, Yellow, Blue, Magenta, Cyan])
  where
    go _ _ S = "S"
    go _ _ K = "K"
    go _ _ I = "I"
    go _ _ B = "B"
    go _ _ C = "C"
    go Parens (c:olors) app@EApp{} = annotate (color c) "(" <> go NoParens olors app <> annotate (color c) ")"
    go NoParens colors (EApp e1 e2)
      = let (hd, args) = collectArgs e1 e2
        in go NoParens colors hd <> align (sep (map (go Parens colors) args))
      where
        collectArgs :: Expr -> Expr -> (Expr, [Expr])
        collectArgs (EApp e e') arg = let (hd, args) = collectArgs e e'
                                      in (hd, args ++ [arg])
        collectArgs hd arg = (hd, [arg])
    go _ [] _ = undefined -- Exhaustiveness checker: cycle is always nonempty

-- | Remove auxiliary definitions I, C, B
removeAuxiliarySymbols :: Expr -> Expr
removeAuxiliarySymbols S = S
removeAuxiliarySymbols K = K
removeAuxiliarySymbols I = unsafeParse "S K K"
removeAuxiliarySymbols B = unsafeParse "S (K S) K"
removeAuxiliarySymbols C = unsafeParse "S (S (K (S (K S) K)) S) (K K)"
removeAuxiliarySymbols (EApp a b) = EApp (removeAuxiliarySymbols a) (removeAuxiliarySymbols b)

normalForm :: Expr -> Expr
normalForm (EApp e x) = case normalForm e of
    I                 -> normalForm x
    EApp K y          -> normalForm y
    EApp (EApp S f) g -> normalForm (EApp (EApp f x) (EApp g x))
    EApp (EApp B f) g -> normalForm (EApp f (EApp g x))
    EApp (EApp C f) y -> normalForm (EApp (EApp f x) y)
    other             -> EApp other x
normalForm S = S
normalForm K = K
normalForm I = I
normalForm B = B
normalForm C = C

type Parser = Parsec Void Text

unsafeParse :: Text -> Expr
unsafeParse input = case parse input of
    Left err -> error ("unsafeParse parse error: " ++ err)
    Right r -> r

parse :: Text -> Either String Expr
parse input = case P.parse (sExprP <* P.eof) ("SK expression" :: String) input of
    Left err -> Left (P.errorBundlePretty err)
    Right r -> Right r

sExprP :: Parser Expr
sExprP = do
    stuff <- P.some term
    case stuff of
        [x]    -> pure x
        s:tuff -> pure (sApp s tuff)
        []     -> undefined -- Exhaustiveness checker: »some« is always nonempty
  where
    sP = tok (P.char' 's') $> S
    kP = tok (P.char' 'k') $> K
    iP = tok (P.char' 'i') $> I
    bP = tok (P.char' 'b') $> B
    cP = tok (P.char' 'c') $> C
    term = asum [sP, kP, iP, bP, cP, parenthesized sExprP]

    sApp :: Expr -> [Expr] -> Expr
    sApp = foldl EApp

tok :: Parser a -> Parser a
tok p = p <* P.space

parenthesized :: Parser a -> Parser a
parenthesized = P.between (tok (P.char '(')) (tok (P.char ')'))
