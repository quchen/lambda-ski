{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module QuasiQuoter (nominal, deBruijn, sk) where



import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Quote

import DeBruijn as B
import Nominal  as N
import Ski      as S



defaultQuoter :: QuasiQuoter
defaultQuoter = QuasiQuoter
    { quoteExp  = \_ -> fail "Quoter not implemented"
    , quotePat  = \_ -> fail "Quoter not implemented"
    , quoteType = \_ -> fail "Quoter not implemented"
    , quoteDec  = \_ -> fail "Quoter not implemented" }

mkQuoter :: (Show err, Lift a) => (Text -> Either err a) -> Text -> QuasiQuoter
mkQuoter parser languageName = defaultQuoter { quoteExp  = expQuoter }
  where
    expQuoter input = case parser (T.pack input) of
        Left err  -> fail (T.unpack ("Invalid " <> languageName <> " expression:\n" <> T.pack (show err)))
        Right ast -> [| ast |]

nominal :: QuasiQuoter
nominal = mkQuoter N.parse "nominal λ calculus"

deBruijn :: QuasiQuoter
deBruijn = mkQuoter B.parse "De Bruijn λ calculus"

sk :: QuasiQuoter
sk = mkQuoter S.parse "SK calculus"
