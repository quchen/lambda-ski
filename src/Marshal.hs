{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Marshal where



import           Control.Applicative
import           Data.Char
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Numeric.Natural

import DeBruijn as B
import Nominal  as N



class ToNominal a where
    toNominal :: a -> N.Expr

newtype FreeVar = FreeVar Text

instance ToNominal FreeVar where
    toNominal (FreeVar name) = N.EVar (Var name)

instance IsString FreeVar where
    fromString = FreeVar . T.pack

natNominal :: Integral a => a -> N.Expr
natNominal n = absN ["f", "x"] (iterate (N.EApp "f") "x" !! fromIntegral n)

instance ToNominal N.Expr where
    toNominal = id
instance ToNominal Bool where
    toNominal True = "λ t _. t"
    toNominal False = "λ _ f. f"
instance ToNominal Int where toNominal = natNominal
instance ToNominal Integer where toNominal = natNominal
instance ToNominal Natural where toNominal = natNominal
instance ToNominal Char where toNominal = natNominal . ord
instance ToNominal Text where toNominal = toNominal . T.unpack

instance ToNominal () where
    toNominal _ = "λ {}. {}"
instance (ToNominal a, ToNominal b) => ToNominal (a,b) where
    toNominal (a, b) = N.EAbs "pair" (appN ["pair", toNominal a, toNominal b])
instance (ToNominal a, ToNominal b, ToNominal c) => ToNominal (a,b,c) where
    toNominal (a, b, c) = toNominal (a, toNominal (b, c))

instance ToNominal a => ToNominal (Maybe a) where
    toNominal Nothing = toNominal ("λ _ Nothing. Nothing" :: N.Expr, toNominal ())
    toNominal (Just x) = toNominal ("λ Just x. Just x" :: N.Expr, toNominal x)

instance (ToNominal a, ToNominal b) => ToNominal (Either a b) where
    toNominal (Left l)  = toNominal ("λ _Left x. x" :: N.Expr, toNominal l)
    toNominal (Right r) = toNominal ("λ Right x. Right x" :: N.Expr, toNominal r)

instance ToNominal a => ToNominal [a] where
    toNominal [] = "λ n _c. n"
    toNominal (x:xs) = N.EApp (N.EApp "λ x list. λ _n c. c x list" (toNominal x)) (toNominal xs)

absN :: [N.Var] -> N.Expr -> N.Expr
absN = foldr (\x xs -> N.EAbs x . xs) id

appN :: [N.Expr] -> N.Expr
appN [f] = f
appN [f,x] = N.EApp f x
appN (f:g:fs) = appN (N.EApp f g : fs)
appN _ = error "Not enough arguments for appN"

class FromDeBruijn a where
    fromDeBruijn :: B.Expr -> Maybe a

instance FromDeBruijn B.Expr where
    fromDeBruijn = Just
instance FromDeBruijn Bool where
    fromDeBruijn (B.EAbs (B.EAbs (B.EVar 1 _))) = Just True
    fromDeBruijn (B.EAbs (B.EAbs (B.EVar 0 _))) = Just False
    fromDeBruijn _ = Nothing
instance FromDeBruijn Int where fromDeBruijn = deBruijnNat
instance FromDeBruijn Integer where fromDeBruijn = deBruijnNat
instance FromDeBruijn Natural where fromDeBruijn = deBruijnNat
instance FromDeBruijn Char where fromDeBruijn = fmap chr . deBruijnNat
instance FromDeBruijn Text where fromDeBruijn = fmap T.pack . (fromDeBruijn :: B.Expr -> Maybe String)

deBruijnNat :: Num a => B.Expr -> Maybe a
deBruijnNat (B.EAbs (B.EAbs n)) = countApps 0 n
  where
    countApps !k (B.EApp (B.EVar 1 _) n') = countApps (k+1) n'
    countApps  k (B.EVar 0 _) = Just k
    countApps  _ _ = Nothing
deBruijnNat _ = Nothing

instance FromDeBruijn a => FromDeBruijn [a] where
    fromDeBruijn (B.EAbs (B.EAbs (B.EVar 1 _))) = Just []
    fromDeBruijn (B.EAbs (B.EAbs (B.EApp (B.EApp (B.EVar 0 _) x) xs))) = liftA2 (:) (fromDeBruijn x) (fromDeBruijn xs)
    fromDeBruijn _ = Nothing

instance FromDeBruijn () where
    fromDeBruijn (B.EAbs (B.EVar 0 _)) = Just ()
    fromDeBruijn _ = Nothing

instance (FromDeBruijn a, FromDeBruijn b) => FromDeBruijn (a,b) where
    fromDeBruijn (B.EAbs (B.EApp (B.EApp (B.EVar 0 _) a) b)) = liftA2 (,) (fromDeBruijn a) (fromDeBruijn b)
    fromDeBruijn _ = Nothing

instance (FromDeBruijn a, FromDeBruijn b, FromDeBruijn c) => FromDeBruijn (a,b,c) where
    fromDeBruijn abc = do
        (a, bc) <- fromDeBruijn abc
        (b, c) <- fromDeBruijn (bc :: B.Expr)
        pure (a, b, c)

instance FromDeBruijn a => FromDeBruijn (Maybe a) where
    fromDeBruijn maybeX = do
        (tag, value) <- fromDeBruijn maybeX
        case (fromDeBruijn tag :: Maybe Int) of
            Just 0 -> pure Nothing
            Just 1 -> fmap Just (fromDeBruijn value)
            _ -> Nothing

instance (FromDeBruijn a, FromDeBruijn b) => FromDeBruijn (Either a b) where
    fromDeBruijn eitherXY = do
        (tag, value) <- fromDeBruijn eitherXY
        case (fromDeBruijn tag :: Maybe Int) of
            Just 0 -> fmap Left (fromDeBruijn value)
            Just 1 -> fmap Right (fromDeBruijn value)
            _ -> Nothing
