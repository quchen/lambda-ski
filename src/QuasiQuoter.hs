{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module QuasiQuoter (nominal) where



import           Data.Either
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Quote

import Nominal as N



defaultQuoter = QuasiQuoter
    { quoteExp  = \_ -> fail "Quoter not implemented"
    , quotePat  = \_ -> fail "Quoter not implemented"
    , quoteType = \_ -> fail "Quoter not implemented"
    , quoteDec  = \_ -> fail "Quoter not implemented" }

mkQuoter parser languageName = defaultQuoter { quoteExp  = expQuoter }
  where
    expQuoter input = case parser (T.pack input) of
        Left err  -> fail (T.unpack ("Invalid " <> languageName <> " expression:\n" <> T.pack (show err)))
        Right ast -> [| ast |]

nominal :: QuasiQuoter
nominal = mkQuoter N.parse "nominal λ calculus"
