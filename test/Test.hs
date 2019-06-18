{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

import Test.Tasty
import Test.Tasty.HUnit

import Convert
import DeBruijn as B
import Nominal  as N



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lambda SKI testsuite"
    [ let eVar ix = B.EVar ix (T.pack "<dummy>")
          eApp = B.EApp
          eAbs = B.EAbs
      in testGroup "Parsing"
        [ testParseDeBruijn Nothing "1" (eVar 1)
        , testParseDeBruijn Nothing "1 2" (eApp (eVar 1) (eVar 2))
        , testParseDeBruijn Nothing "1 2 3" (eApp (eApp (eVar 1) (eVar 2)) (eVar 3))
        , testParseDeBruijn
            (Just "Y = λ(λ1(0 0)) (λ1(0 0))")
            "λ(λ1(0 0)) (λ1(0 0))"
            (eAbs (eApp (eAbs (eApp (eVar 1)
                                    (eApp (eVar 0)
                                          (eVar 0))))
                        (eAbs (eApp (eVar 1)
                                    (eApp (eVar 0)
                                          (eVar 0))))))
        ]
    , testGroup "Conversions between representations"
        [ testGroup "Nominal → De Bruijn"
            [ testNominalToDeBruijn
                "λx y. x"
                "λλ1"
            , testNominalToDeBruijn
                "λx y. y"
                "λλ0"
            , testNominalToDeBruijn "λok. (λx. x) ok" "(λ(λ0)0)"
            , testNominalToDeBruijn
                "λf. (λx. f (x x)) (λx. f (x x))"
                "(λ(λ1(0 0)) (λ1(0 0)))"
            ]
        , testGroup "Nominal → De Bruijn → Nominal"
            [ testNominalToDeBruijnAndBack Nothing "λx y z.z y x"
            , testNominalToDeBruijnAndBack Nothing "λ_ x.x"
            , testNominalToDeBruijnAndBack Nothing "λok. (λx. x) ok"
            , testNominalToDeBruijnAndBack (Just "Y") "λf. (λx. f (x x)) (λx. f (x x))"
            , testNominalToDeBruijnAndBack (Just "factorial")
                " (λ-1 * true false Y 1.                      \
                \     (λ =0.                                  \
                \         Y (λrec n. (=0 n)                   \
                \                    1                        \
                \                    (* n (rec (-1 n)))       \
                \           )                                 \
                \     )                                       \
                \     (λn. n (λ_. false) true)                \
                \ )                                           \
                \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) \
                \ (λm n f x. m (n f) x)                       \
                \ (λt _. t)                                   \
                \ (λ_ f. f)                                   \
                \ (λf. (λx. f (x x)) (λx. f (x x)))           \
                \ (λf x. f x)                                 "
            , testNominalToDeBruijnAndBack (Just "Fibonacci")
                " (λ +1 -1 true false Y 1.                       \
                \     (λ =0 + -.                                 \
                \         (λ <= 2.                               \
                \             Y (λrec n. (<= n 1)                \
                \                        n                       \
                \                        (+ (rec (- n 1))        \
                \                                (rec (- n 2)))) \
                \         )                                      \
                \         (λm n. =0 (- m n))                     \
                \         (+ 1 1)                                \
                \     )                                          \
                \     (λn. n (λ_. false) true)                   \
                \     (λm n. m +1 n)                             \
                \     (λm n. n -1 m)                             \
                \ )                                              \
                \ (λn f x. f (n f x))                            \
                \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u))    \
                \ (λt _. t)                                      \
                \ (λ_ f. f)                                      \
                \ (λf. (λx. f (x x)) (λx. f (x x)))              \
                \ (λf x. f x)                                    "
            ]
        ]
    , testGroup "Evaluation"
        [ testGroup "De Bruijn ⇝ De Bruijn"
            [ testReduceDeBruijn Nothing "1" "1"
            , testReduceDeBruijn Nothing "1 2" "1 2"
            , testReduceDeBruijn Nothing "1 2 3" "1 2 3"
            , testReduceDeBruijn Nothing "(λ0)1" "1"
            , testReduceDeBruijn Nothing "λ0(λ0)" "λ0(λ0)"
            , testReduceDeBruijn Nothing "(λ 0) ((λ 0) 1)" "1"
            , testReduceDeBruijn Nothing "(λ 0) (λ 0) 1" "1"
            , testReduceDeBruijn
                (Just "Y (const 123)")
                "(λ(λ1(0 0)) (λ1(0 0))) (λ123)"
                "122"
            , testReduceDeBruijn
                (Just "const 123 Ω")
                "(λλ1) 123 ((λ0 0)(λ0 0))"
                "123"
            ]
        , testGroup "Nominal → De Bruijn ⇝ De Bruijn → Nominal"
            [ testReduceNominalViaDeBruijn Nothing "(λx. x) (λok. ok)" "(λok. ok)"
            , testReduceNominalViaDeBruijn
                (Just "Y (const ok)")
                "(λf. (λx. f (x x)) (λx. f (x x))) (λ_. (λok. ok))"
                "λok. ok"
            , testReduceNominalViaDeBruijn
                (Just "2 + 1")
                " (λ+1 1 2.           \
                \     (λ+.            \
                \         + 2 1       \
                \     )               \
                \     (λm n. m +1 n)  \
                \ )                   \
                \ (λn f x. f (n f x)) \
                \ (λf x. f x)         \
                \ (λf x. f (f x))     "
                "λf x. f (f (f x))"
            , testReduceNominalViaDeBruijn
                (Just "factorial(3)")
                " (λ-1 * true false Y 1.                      \
                \     (λ =0.                                  \
                \         Y (λrec n. (=0 n)                   \
                \                    1                        \
                \                    (* n (rec (-1 n)))       \
                \           )                                 \
                \     )                                       \
                \     (λn. n (λ_. false) true)                \
                \ )                                           \
                \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) \
                \ (λm n f x. m (n f) x)                       \
                \ (λt _. t)                                   \
                \ (λ_ f. f)                                   \
                \ (λf. (λx. f (x x)) (λx. f (x x)))           \
                \ (λf x. f x)                                 \
                \ (λf x. f (f (f x)))                         "
                "λf x. f (f (f (f (f (f x)))))"
            , testReduceNominalViaDeBruijn
                (Just "fibonacci(6)")
                " (λ +1 -1 true false Y 1.                       \
                \     (λ =0 + -.                                 \
                \         (λ <= 2.                               \
                \             Y (λrec n. (<= n 1)                \
                \                        n                       \
                \                        (+ (rec (- n 1))        \
                \                                (rec (- n 2)))) \
                \         )                                      \
                \         (λm n. =0 (- m n))                     \
                \         (+ 1 1)                                \
                \     )                                          \
                \     (λn. n (λ_. false) true)                   \
                \     (λm n. m +1 n)                             \
                \     (λm n. n -1 m)                             \
                \ )                                              \
                \ (λn f x. f (n f x))                            \
                \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u))    \
                \ (λt _. t)                                      \
                \ (λ_ f. f)                                      \
                \ (λf. (λx. f (x x)) (λx. f (x x)))              \
                \ (λf x. f x)                                    \
                \ (λf x. f (f (f (f (f (f x))))))                "
                "λf x. f (f (f (f (f (f (f (f x)))))))"
            ]
        ]
        , testGroup "Hello, world!"
            [ testHelloWorld
            ]
    ]

testParseDeBruijn :: Maybe TestName -> Text -> B.Expr -> TestTree
testParseDeBruijn mTestName input expected = testCase testName test
  where
    testName = fromMaybe (T.unpack input) mTestName
    test = assertEqual "" expected (B.unsafeParse input)

testNominalToDeBruijn :: Text -> Text -> TestTree
testNominalToDeBruijn inputSrc expectedSrc = testCase testName test
  where
    testName = T.unpack inputSrc
    expected = B.unsafeParse expectedSrc
    actual = unsafeNominalToDeBruijn (N.unsafeParse inputSrc)
    test = assertEqual "" expected actual

testNominalToDeBruijnAndBack :: Maybe TestName -> Text -> TestTree
testNominalToDeBruijnAndBack mTestName inputSrc = testCase testName test
  where
    testName = fromMaybe (T.unpack inputSrc) mTestName
    nominal = N.unsafeParse inputSrc
    nominalAgain = deBruijnToNominal (unsafeNominalToDeBruijn nominal)
    test = assertEqual "" nominal nominalAgain

testReduceDeBruijn :: Maybe TestName -> Text -> Text -> TestTree
testReduceDeBruijn mTestName inputSrc expectedSrc = testCase testName test
  where
    testName = fromMaybe (T.unpack inputSrc) mTestName
    actual = eval (B.unsafeParse inputSrc)
    expected = B.unsafeParse expectedSrc
    test = assertEqual "" expected actual

testReduceNominalViaDeBruijn :: Maybe TestName -> Text -> Text -> TestTree
testReduceNominalViaDeBruijn mTestName inputSrc expectedSrc = testCase testName test
  where
    testName = fromMaybe (T.unpack inputSrc) mTestName
    actual = (deBruijnToNominal . eval . unsafeNominalToDeBruijn . N.unsafeParse) inputSrc
    expected = N.unsafeParse expectedSrc
    test = assertEqual "" expected actual

testHelloWorld :: TestTree
testHelloWorld = testCase "Lambda calculus version" test
  where
    test = assertEqual "" expected actual
    expected = "Hello, world!\n"
    actual = marshal (deBruijnToNominal (normalForm (unsafeNominalToDeBruijn helloWorld)))

    marshal :: N.Expr -> String
    marshal (N.EAbs _ e) = marshal e
    marshal (N.EApp (N.EApp (N.EVar (Var "hask_outChr")) increments) cont)
      = let char (N.EApp (N.EVar (Var "hask_succ")) rest) = succ (char rest)
            char (N.EVar (Var "hask_0")) = minBound
            char nope = error ("Bad increment: " ++ take 32 (show nope))
        in char increments : marshal cont
    marshal (N.EVar (Var "hask_eof")) = ""
    marshal nope = error ("Marshalling broken or bad λAST: " ++ take 32 (show nope))

    helloWorld :: N.Expr
    helloWorld = N.unsafeParse
        " (λ hask_outChr hask_succ hask_0 hask_eof.                                                     \
        \     (λ 1 + *.                                                                                 \
        \         (λ2 ^.                                                                                \
        \             (λ4 8 16 32 64 print.                                                             \
        \                 (λ H e l o , space w r d ! newline.                                           \
        \                      H (e (l (l (o (, (space (w (o (r (l (d (! (newline hask_eof))))))))))))) \
        \                 )                                                                             \
        \                 (print (+ 64 8))                                                              \
        \                 (print (+ 1 (+ 4 (+ 32 64))))                                                 \
        \                 (print (+ 4 (+ 8 (+ 32 64))))                                                 \
        \                 (print (+ 1 (+ 2 (+ 4 (+ 8 (+ 32 64))))))                                     \
        \                 (print (+ 4 (+ 8 32)))                                                        \
        \                 (print 32)                                                                    \
        \                 (print (+ 1 (+ 2 (+ 4 (+ 16 (+ 32 64))))))                                    \
        \                 (print (+ 2 (+ 16 (+ 32 64))))                                                \
        \                 (print (+ 4 (+ 32 64)))                                                       \
        \                 (print (+ 1 32))                                                              \
        \                 (print (+ 2 8))                                                               \
        \             )                                                                                 \
        \             (^ 2 2)                                                                           \
        \             (^ 2 (+ 1 2))                                                                     \
        \             (^ 2 (+ 2 2))                                                                     \
        \             (^ 2 (+ 1 (+ 2 2)))                                                               \
        \             (^ 2 (+ 2 (+ 2 2)))                                                               \
        \             (λn. hask_outChr (n hask_succ hask_0))                                            \
        \         )                                                                                     \
        \         (+ 1 1)                                                                               \
        \         (λb e. e (* b) 1)                                                                     \
        \     )                                                                                         \
        \     (λf x. f x)                                                                               \
        \     (λa b. λf x. a f (b f x))                                                                 \
        \     (λa b. λf x. a (b f) x)                                                                   \
        \ )                                                                                             "
