{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans where



import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Text.Megaparsec as P



instance P.ShowErrorComponent Text where
    showErrorComponent = T.unpack
