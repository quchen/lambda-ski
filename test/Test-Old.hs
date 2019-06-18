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
tests = testGroup "Lambda SKI testsuite"
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
            (unsafeParseLambda "(λx y. y x) y z")
        ]
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
            (unsafeParseLambda "(λx y. x) (λx. f x) (λx. x x x)")
        ]
    , testProgram "Y (const 1)"
        (unsafeParseLambda "1")
        (unsafeParseLambda "(λY. Y (λx y. x)) (λf. (λx. f (x x)) (λx. f (x x)))")

    ,   let n = 5
            factorial :: Natural -> Natural
            factorial k = product [1..k]
        in testProgram (show n ++ "! = " ++ show (factorial n))
            (LApp
                (unsafeParseLambda "\
                    \ (λpred mul true false Y.                    \
                    \     (λisZero.                               \
                    \         Y (λrec n. (isZero n)               \
                    \                    n                        \
                    \                    (mul n (rec (pred n)))   \
                    \           )                                 \
                    \     )                                       \
                    \     (λn. n (λ_. false) true)                \
                    \ )                                           \
                    \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) \
                    \ (λm n f x. m (n f) x)                       \
                    \ (λt _. t)                                   \
                    \ (λ_ f. f)                                   \
                    \ (λf. (λx. f (x x)) (λx. f (x x)))           \
                    \ (λf x. f (f (f x)))                         ")
                (nat n))
            (nat (factorial n))

    ,   let n = 5
            fibo k = let fib :: [Natural]
                         fib = 0 : 1 : zipWith (+) (tail fib) fib
                     in fib !! fromIntegral k
        in testProgram ("fibo " ++ show n ++ " = " ++ show (fibo n))
            (LApp
                (unsafeParseLambda
                    " (λsucc pred true false Y 1 2 isZero add sub leq. \
                    \     Y (λrec n. (leq n 1)                         \
                    \                n                                 \
                    \                (add (rec (sub n 1))              \
                    \                         (rec (sub n 2)))))       \
                    \ (λn f x. f (n f x))                              \
                    \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u))      \
                    \ (λt _. t)                                        \
                    \ (λ_ f. f)                                        \
                    \ (λf. (λx. f (x x)) (λx. f (x x)))                \
                    \ (λf x. f x)                                      \
                    \ (λf x. f (f x))                                  \
                    \ (λn. n (λ_. false) true)                         \
                    \ (λm n. m succ n)                                 \
                    \ (λm n. n pred m)                                 \
                    \ (λm n. isZero (sub m n))                         ")
                (nat n))
            (nat (fibo n))
    ]
  where
    testProgram
        :: TestName
        -> LExpr Var -- ^ Expected reduct
        -> LExpr Var -- ^ Source
        -> TestTree
    testProgram name expected input
      = testCase name (assertEqual "" expected (evalLambda input))

nat :: Natural -> LExpr Var
nat = LAbs "f" . LAbs "x" . go
  where
    go 0 = LVar "x"
    go n = LApp (LVar "f") (go (n-1))
