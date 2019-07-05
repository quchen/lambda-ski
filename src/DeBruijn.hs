{-# LANGUAGE OverloadedStrings #-}

module DeBruijn (
    Expr(..),
    eval,
    normalForm,
    parse,
    unsafeParse,
    prettyAnsi
) where



import           Control.Applicative
import           Data.Char
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Void
import           Numeric.Natural
import           Text.Megaparsec                           (Parsec, (<?>))
import qualified Text.Megaparsec                           as P
import qualified Text.Megaparsec.Char                      as P
import           Text.Read                                 (readMaybe)



data Expr
    = EVar !Natural Text -- ^ Bound variable
    | EVarFree Text      -- ^ Free variable
    | EApp Expr Expr     -- ^ Function application
    | EAbs Expr          -- ^ λ abstraction

instance Eq Expr where
    EVar n _ == EVar m _ = n == m
    EApp f1 x1 == EApp f2 x2 = (f1, x1) == (f2, x2)
    EAbs x == EAbs y = x == y
    _ == _ = False

instance Show Expr where
    -- show (EVar n) = "\x1b[" ++ show (mod n 6 + 31) ++ "m" ++ show n ++ "\x1b[m"
    -- show (EApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    -- show (EAbs e) = "λ " ++ show e

    -- showsPrec _ (EVar n _name) = showString ("\x1b[" ++ show (mod n 6 + 31) ++ "m" ++ show n ++ "\x1b[m")
    showsPrec _ (EVar n _name) = shows n
    showsPrec _ (EVarFree name) = shows (T.unpack name)
    showsPrec p (EApp e1 e2) = showParen (p > 10)
        (showsPrec 10 e1 . showChar ' ' . showsPrec (10+1) e2)
    showsPrec p (EAbs e) = showParen (p > 5) (showChar 'λ' . spacer . showsPrec 5 e)
      where
        spacer = case e of
            EAbs{} -> id
            _other -> showChar ' '

instance Pretty Expr where
    pretty = unAnnotate . prettyAnsi

prettyAnsi :: Expr -> Doc AnsiStyle
prettyAnsi = prettyPrec 0

prettyPrec :: Int -> Expr -> Doc AnsiStyle
prettyPrec _ (EVar n _name) = annotate (styleByIndex n) (pretty n)
prettyPrec _ (EVarFree name) = annotate (styleByIndex (nameHash name)) (pretty name)
  where
    nameHash :: Text -> Natural
    nameHash = T.foldl' (\acc char -> fromIntegral (ord char) + acc) 0
prettyPrec p (EApp e1 e2) = parenthesize (p > 10)
    (align (sep [prettyPrec 10 e1, prettyPrec (10+1) e2]))
prettyPrec p (EAbs e) = parenthesize (p > 5)
    ("λ" <> spacer <>  prettyPrec 5 e)
  where
    spacer = case e of
        EAbs{} -> mempty
        _other -> space

styleByIndex :: Natural -> AnsiStyle
styleByIndex n = case mod n 6 of
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

eval :: Expr -> Expr
eval = normalForm

normalForm :: Expr -> Expr
normalForm (EApp f x) = case normalForm f of
    EAbs body -> normalForm (shift (-1) 0 (subst 0 (shift 1 0 x) body))
    f' -> EApp f' (normalForm x)
normalForm (EAbs e) = EAbs (normalForm e)
normalForm var@EVar{} = var
normalForm var@EVarFree{} = var

shift
    :: Integer -- ^ Shift amount
    -> Natural -- ^ Cutoff: only vars >= will be shifted
    -> Expr
    -> Expr
shift amount cutoff var@(EVar k name)
    | k >= cutoff = EVar (fromInteger (fromIntegral k + amount)) name
    | otherwise = var
shift amount cutoff (EAbs e) = EAbs (shift amount (cutoff+1) e)
shift amount cutoff (EApp f x) = EApp (shift amount cutoff f) (shift amount cutoff x)
shift _ _ var@EVarFree{} = var

subst
    :: Natural -- ^ Index
    -> Expr -- ^ Value to insert at index
    -> Expr -- ^ Body
    -> Expr
subst i x var@(EVar i' _name)
    | i == i' = x
    | otherwise = var
subst i x (EAbs e) = EAbs (subst (i+1) (shift 1 0 x) e)
subst i x (EApp e1 e2) = EApp (subst i x e1) (subst i x e2)
subst _ _ var@EVarFree{} = var

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
        []     -> error "Impossible branch: some x is always nonempty"
        [x]    -> pure x
        s:tuff -> pure (lApp s tuff)
  where
    termP :: Parser Expr
    termP = eVarP <|> eAbsP <|> parenthesized eExprP

    eAbsP :: Parser Expr
    eAbsP = do
        _lambda <- tok (P.oneOf ("λ\\" :: [Char])) <?> "lambda"
        body    <- eExprP
        pure (EAbs body)

    eVarP :: Parser Expr
    eVarP = do
        let dummyName = "<dummy>"
        ix <- tok (P.some P.digitChar)
        case readMaybe ix of
            Just ix' -> pure (EVar ix' dummyName)
            Nothing -> fail "Parse error"

    lApp :: Expr -> [Expr] -> Expr
    lApp = foldl EApp

tok :: Parser a -> Parser a
tok p = p <* P.space

parenthesized :: Parser a -> Parser a
parenthesized = P.between (tok (P.char '(')) (tok (P.char ')'))
