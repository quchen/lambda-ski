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
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Void
import           Text.Megaparsec                           (Parsec)
import qualified Text.Megaparsec                           as P
import qualified Text.Megaparsec.Char                      as P
import Data.String

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

    | EFree Text -- ^ Free variable

    | EApp Expr Expr
    deriving (Eq, Ord)

instance IsString Expr where
    fromString = unsafeParse . T.pack

infixl 9 `EApp`

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
    go _ _ (EFree name) = annotate (styleByIndex (nameHash name)) (pretty name)
      where
        nameHash = T.foldl' (\acc char -> fromIntegral (ord char) + acc) 0
    go Parens (c:olors) app@EApp{} = annotate (color c) "(" <> go NoParens olors app <> annotate (color c) ")"
    go NoParens colors (EApp e1 e2)
      = let (hd, args) = collectArgs e1 e2
        in go NoParens colors hd <+> align (sep (map (go Parens colors) args))
      where
        collectArgs :: Expr -> Expr -> (Expr, [Expr])
        collectArgs (EApp e e') arg = let (hd, args) = collectArgs e e'
                                      in (hd, args ++ [arg])
        collectArgs hd arg = (hd, [arg])
    go _ [] _ = undefined -- Exhaustiveness checker: cycle is always nonempty

styleByIndex :: Int -> AnsiStyle
styleByIndex n = case mod n 6 of
    0 -> color Red
    1 -> color Green
    2 -> color Yellow
    3 -> color Blue
    4 -> color Magenta
    5 -> color Cyan
    _ -> error "Cannot happen because of modulus"

-- | Remove auxiliary definitions I, C, B
removeAuxiliarySymbols :: Expr -> Expr
removeAuxiliarySymbols S = S
removeAuxiliarySymbols K = K
removeAuxiliarySymbols I = "S K K"
removeAuxiliarySymbols B = "S (K S) K"
removeAuxiliarySymbols C = "S (S (K (S (K S) K)) S) (K K)"
removeAuxiliarySymbols free@EFree{} = free
removeAuxiliarySymbols (EApp a b) = EApp (removeAuxiliarySymbols a) (removeAuxiliarySymbols b)

normalForm :: Expr -> Expr
normalForm (EApp e x) = case normalForm e of
    I                   -> normalForm x
    EApp K y            -> normalForm y
    S `EApp` f `EApp` g -> normalForm (f `EApp` x `EApp` (g `EApp` x))
    B `EApp` f `EApp` g -> normalForm (f `EApp` (g `EApp` x))
    C `EApp` f `EApp` y -> normalForm (f `EApp` x `EApp` y)
    other               -> EApp other x
normalForm free@EFree{} = free
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
    atom = do
        name <- tok (P.some (P.satisfy (\c -> not (isSpace c || elem c ("()" :: String)))))
        pure (case name of
            "s" -> S
            "k" -> K
            "i" -> I
            "b" -> B
            "c" -> C
            free -> EFree (T.pack free))
    term = atom P.<|> parenthesized sExprP

    sApp :: Expr -> [Expr] -> Expr
    sApp = foldl EApp

tok :: Parser a -> Parser a
tok p = p <* P.space

parenthesized :: Parser a -> Parser a
parenthesized = P.between (tok (P.char '(')) (tok (P.char ')'))
