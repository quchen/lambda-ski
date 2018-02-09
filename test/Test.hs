{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Lib

import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Numeric.Natural

import Test.Tasty
import Test.Tasty.HUnit



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lambda SKI testsuist"
    [ testSubstitution
    , testExamplePrograms
    ]

testSubstitution :: TestTree
testSubstitution = testGroup "Substitution"
    [ testSubstitute "x" "y" "a b x c" "a b y c"
    , testSubstitute "x" "y" "λx.x y" "λx.x y"
    , testSubstitute "y" "z" "λx.x y" "λx.x z"
    , testSubstitute "x" "z" "x (λx.x y) x" "z (λx.x y) z"
    ]

  where
    testSubstitute :: Var -> Var -> Text -> Text -> TestTree
    testSubstitute before after inputRaw expectedRaw
      = let input = unsafeParseLambda inputRaw
            expected = unsafeParseLambda expectedRaw
            test = assertEqual "" expected (substitute before after input)
            testName = "(" <> T.unpack inputRaw <> ") [" <> show before <> " → " <> show after <> "]"
        in testCase testName test

testExamplePrograms :: TestTree
testExamplePrograms = (localOption (Timeout 1e6 "1s") . testGroup "Normal form reduction")
    [ testGroup "Sanity tests"
        [ testProgram "free variable"
            "x"
            "x"
        , testProgram "free variable applied to itself"
            "x x"
            "x x"
        , testProgram "plain lambda"
            "λx. x"
            "λx. x"
        , testProgram "id"
            "1"
            "(λx. x) 1"
        , testProgram "const"
            "1"
            "(λx y. x) 1 2"
        ]
    , testGroup "Aliasing"
        [ testProgram "direct"
            "2"
            "(λx x. x) 1 2"
        , testProgram "indirect (created by substitution)"
            "z y"
            "(λx y. y x) y z"]
    -- , testGroup "Flips that used to fail :-)"
    --     [ testProgram "single flip"
    --         "f x"
    --         "(λx y. y x) y z"
    --     ,testProgram "single flip"
    --         "f x"
    --         "(λflip. flip (λx y. g y x)) (λf x y. f y x)"
    --     , testProgram "inlined double flip"
    --         "f x"
    --         "(λf x y. f y x) ((λf x y. f y x) f)"
    --     , testProgram "double flip"
    --         "f x"
    --         "(λflip. flip (flip f)) (λf x y. f y x)"
    --     , testProgram "non-variable as replacement"
    --         "λx. f x"
    --         "(λx y. x) (λx. f x) (λx. x x x)")
    -- , testProgram "Y (const 1)"
    --     "1"
    --     "(λY. Y (λx y. x)) (λf. (λx. f (x x)) (λx. f (x x)))"
    ]

testProgram
    :: TestName
    -> Text -- ^ Expected reduct
    -> Text -- ^ Source
    -> TestTree
testProgram name expected input
  = testCase name
             (assertEqual ""
                          (unsafeParseLambda expected)
                          (evalLambda (unsafeParseLambda input)))

nat :: Natural -> LExpr Var
nat = LAbs "f" . LAbs "x" . go
  where
    go 0 = LVar "x"
    go n = LApp (LVar "f") (go (n-1))
