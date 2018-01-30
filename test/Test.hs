{-# LANGUAGE NumDecimals       #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Lib

import Test.Tasty
import Test.Tasty.HUnit



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lambda SKI tests"
    [ testExamplePrograms
    ]

testExamplePrograms :: TestTree
testExamplePrograms = (localOption (Timeout 1e6 "1s") . testGroup "Full programs")
    [ testGroup "Sanity tests"
        [ testProgram "free variable"
            (unsafeParseLambda "x")
            (unsafeParseLambda "x")
        , testProgram "free variable applied to itself"
            (unsafeParseLambda "x x")
            (unsafeParseLambda "x x")
        , testProgram "plain lambda"
            (unsafeParseLambda "λx. x")
            (unsafeParseLambda "λx. x")
        , testProgram "id"
            (unsafeParseLambda "1")
            (unsafeParseLambda "(λx. x) 1")
        , testProgram "const"
            (unsafeParseLambda "1")
            (unsafeParseLambda "(λx y. x) 1 2")
        ]
    , testGroup "Aliasing"
        [ testProgram "direct"
            (unsafeParseLambda "2")
            (unsafeParseLambda "(λx x. x) 1 2")
        , testProgram "indirect (created by substitution)"
            (unsafeParseLambda "z y")
            (unsafeParseLambda "(λx y. y x) y z") ]
    , testGroup "Flips that used to fail :-)"
        [ testProgram "single flip"
            (unsafeParseLambda "f x")
            (unsafeParseLambda "(λx y. y x) y z")
        ,testProgram "single flip"
            (unsafeParseLambda "f x")
            (unsafeParseLambda "(λflip. flip (λx y. g y x)) (λf x y. f y x)")
        , testProgram "inlined double flip"
            (unsafeParseLambda "f x")
            (unsafeParseLambda "(λf x y. f y x) ((λf x y. f y x) f)")
        , testProgram "double flip"
            (unsafeParseLambda "f x")
            (unsafeParseLambda "(λflip. flip (flip f)) (λf x y. f y x)")
        , testProgram "non-variable as replacement"
            (unsafeParseLambda "λx. f x")
            (unsafeParseLambda "(λx y. x) (λx. f x) (λx. x x x)") ]
    -- , testProgram "Y (const 1)"
    --     (unsafeParseLambda "1")
    --     (unsafeParseLambda "(λY. Y (λx y. x)) (λf. (λx. f (x x)) (λx. f (x x)))")
    ]

testProgram
    :: TestName
    -> LExpr -- ^ Expected reduct
    -> LExpr -- ^ Source
    -> TestTree
testProgram name expected input
  = testCase name (assertEqual "" expected (evalLambda input))
