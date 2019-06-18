{-# LANGUAGE OverloadedStrings #-}

module Ski where



import           Control.Applicative
import           Data.Functor
import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Void
import           Text.Megaparsec           (Parsec)
import qualified Text.Megaparsec           as P
import qualified Text.Megaparsec.Char      as P

import Orphans ()



data SExpr
    = S
    | K
    | SApp SExpr SExpr
    deriving (Eq, Ord)

data ParensNecessary = Parens | NoParens

instance Pretty SExpr where
    pretty = go NoParens
      where
        go _ S = "S"
        go _ K = "K"
        go Parens app@SApp{} = parens (go NoParens app)
        go NoParens (SApp x y) = align (sep [go NoParens x, go Parens y])

type Parser = Parsec Void Text

parseSki :: Text -> Either String SExpr
parseSki input = case P.parse (sExprP <* P.eof) ("SK expression" :: String) input of
    Left err -> Left (P.errorBundlePretty err)
    Right r -> Right r

sExprP :: Parser SExpr
sExprP = do
    stuff <- P.some term
    case stuff of
        []     -> error "some is broken booo"
        [x]    -> pure x
        s:tuff -> pure (sApp s tuff)
  where
    sP = tok (P.char' 's') $> S
    kP = tok (P.char' 'k') $> K
    term = sP <|> kP <|> parenthesized sExprP

    sApp :: SExpr -> [SExpr] -> SExpr
    sApp = foldl SApp

tok :: Parser a -> Parser a
tok p = p <* P.space

parenthesized :: Parser a -> Parser a
parenthesized = P.between (tok (P.char '(')) (tok (P.char ')'))
