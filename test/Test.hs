{-# LANGUAGE OverloadedStrings #-}

module Main (main) where



import           Data.Maybe
import           Data.Text  (Text)
import qualified Data.Text  as T

import Test.Tasty
import Test.Tasty.HUnit

import Convert
import DeBruijn        as B
import ExamplePrograms
import Nominal         as N
import Ski             as S



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Lambda SKI testsuite"
    [ testGroup "Parsing"
        [ testGroup "Nominal"
            [ testParseShowInverse "factorial" (showT factorial) N.parse
            , testParseShowInverse "fibonacci" (showT fibonacci) N.parse
            , testParseShowInverse "Hello World" (showT helloWorld) N.parse
            ]
        , testGroup "De Bruijn"
            [ let eVar ix = B.EVar ix (T.pack "<dummy>")
                  eApp = B.EApp
                  eAbs = B.EAbs
                  eFree = B.EVarFree
              in testGroup "Handwritten"
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
                , testParseDeBruijn
                    Nothing
                    "λ (λ 1 0 (free 0))"
                    (eAbs (eAbs (eApp (eApp (eVar 1) (eVar 0)) (eApp (eFree "free") (eVar 0)))))
                ]
            , testGroup "Example programs"
                [ testParseShowInverse "factorial" (showT (nominalToDeBruijn factorial)) B.parse
                , testParseShowInverse "fibonacci" (showT (nominalToDeBruijn fibonacci)) B.parse
                , testParseShowInverse "Hello World" (showT (nominalToDeBruijn helloWorld)) B.parse
                ]
            ]
        , testGroup "SK"
            [ testGroup "Example programs"
                [ testParseShowInverse "Y combinator" ("Y = S S K (S (K (S S (S (S S K)))) K)") S.parse
                , testParseShowInverse "factorial" (showT (nominalToSki factorial)) S.parse
                , testParseShowInverse "fibonacci" (showT (nominalToSki fibonacci)) S.parse
                , testParseShowInverse "Hello World" (showT (nominalToSki helloWorld)) S.parse
                ]
            ]
        ]
    , testGroup "Conversions between representations"
        [ testGroup "Nominal → De Bruijn"
            [ testNominalToDeBruijn
                "λx y. x"
                "λλ1"
            , testNominalToDeBruijn
                "λx y. y"
                "λλ0"
            , testNominalToDeBruijn "λy. (λx. x) y" "(λ(λ0)0)"
            , testNominalToDeBruijn
                "λf. (λx. f (x x)) (λx. f (x x))"
                "(λ(λ1(0 0)) (λ1(0 0)))"
            ]
        , testGroup "Nominal → De Bruijn → Nominal"
            [ testNominalToDeBruijnAndBack Nothing "λx y z.z y x"
            , testNominalToDeBruijnAndBack Nothing "λ_ x.x"
            , testNominalToDeBruijnAndBack Nothing "λ_. free"
            , testNominalToDeBruijnAndBack Nothing "λbound. (λ_. free) bound free"
            , testNominalToDeBruijnAndBack (Just "Y") "λf. (λx. f (x x)) (λx. f (x x))"
            , testNominalToDeBruijnAndBack (Just "factorial") factorial
            , testNominalToDeBruijnAndBack (Just "Fibonacci") fibonacci
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
            [ testReduceNominalViaDeBruijn
                Nothing
                "(λx. x) ok"
                "ok"
            , testReduceNominalViaDeBruijn
                (Just "Y (const ok)")
                "(λf. (λx. f (x x)) (λx. f (x x))) (λ_. ok)"
                "ok"
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
            , let n = 5
                  fac k = product [1..k]
              in testReduceNominalViaDeBruijn
                (Just ("factorial(" ++ show n ++ ")"))
                (N.EApp factorial (nat n))
                (nat (fac n))
            , let n = 8
                  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                  fib k = fibs !! k
              in testReduceNominalViaDeBruijn
                (Just ("fibonacci(" ++ show n ++ ")"))
                (N.EApp fibonacci (nat n))
                (nat (fib n))
            ]
        ]
        , testGroup "Nominal → SKICB ⇝ SKICB"
            [ testReduceSki
                Nothing
                "(λx. x) ok"
                "ok"
            , testReduceSki
                (Just "Y (const ok)")
                "(λf. (λx. f (x x)) (λx. f (x x))) (λ_. ok)"
                "ok"
            , testReduceSki
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
            ]
        , testGroup "Hello, world!"
            [ testHelloWorldNominal
            , testHelloWorldSki
            ]
    ]

testParseDeBruijn :: Maybe TestName -> Text -> B.Expr -> TestTree
testParseDeBruijn mTestName input expected = testCase testName test
  where
    testName = fromMaybe (T.unpack input) mTestName
    test = assertEqual "" expected (B.unsafeParse input)

testNominalToDeBruijn :: N.Expr -> B.Expr -> TestTree
testNominalToDeBruijn input expected = testCase testName test
  where
    testName = show input
    actual = nominalToDeBruijn input
    test = assertEqual "" expected actual

testNominalToDeBruijnAndBack :: Maybe TestName -> N.Expr -> TestTree
testNominalToDeBruijnAndBack mTestName nominal = testCase testName test
  where
    testName = fromMaybe (show nominal) mTestName
    nominalAgain = deBruijnToNominal (nominalToDeBruijn nominal)
    test = assertEqual "" nominal nominalAgain

testReduceDeBruijn :: Maybe TestName -> B.Expr -> B.Expr -> TestTree
testReduceDeBruijn mTestName input expected = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = eval input
    test = assertEqual "" expected actual

testReduceNominalViaDeBruijn :: Maybe TestName -> N.Expr -> N.Expr -> TestTree
testReduceNominalViaDeBruijn mTestName input expected = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = (deBruijnToNominal . eval . nominalToDeBruijn) input
    test = assertEqual "" expected actual

testReduceSki :: Maybe TestName -> N.Expr -> N.Expr -> TestTree
testReduceSki mTestName input expectedNominal = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = S.normalForm (nominalToSki input)
    expected = nominalToSki expectedNominal
    test = assertEqual "" expected actual

testHelloWorldNominal :: TestTree
testHelloWorldNominal = testCase "Lambda calculus version, old implementation" test
  where
    test = assertEqual "" expected actual
    expected = "Hello, world!\n"
    actual = marshal (deBruijnToNominal (B.normalForm (nominalToDeBruijn source)))

    marshal :: N.Expr -> String
    marshal (N.EAbs _ e) = marshal e
    marshal (N.EApp (N.EApp (N.EVar (Var "hask_outChr")) increments) cont)
      = let char (N.EApp (N.EVar (Var "hask_succ")) rest) = succ (char rest)
            char (N.EVar (Var "hask_0")) = minBound
            char nope = error ("Bad increment: " ++ take 32 (show nope))
        in char increments : marshal cont
    marshal (N.EVar (Var "hask_eof")) = ""
    marshal nope = error ("Marshalling broken or bad λAST: " ++ take 32 (show nope))

    source :: N.Expr
    source = helloWorld

testHelloWorldSki :: TestTree
testHelloWorldSki = testCase "SKI calculus" test
  where
    test = assertEqual "" expected actual
    expected = "Hello, world!\n"
    actual = marshal (skiToNominal (S.normalForm (nominalToSki helloWorld)))

    marshal :: N.Expr -> String
    marshal (N.EAbs _ e) = marshal e
    marshal (N.EApp (N.EApp (N.EVar (Var "hask_outChr")) increments) cont)
      = let char (N.EApp (N.EVar (Var "hask_succ")) rest) = succ (char rest)
            char (N.EVar (Var "hask_0")) = minBound
            char nope = error ("Bad increment: " ++ take 128 (show nope))
        in char increments : marshal cont
    marshal (N.EVar (Var "hask_eof")) = ""
    marshal nope = error ("Cannot marshal value: " ++ take 128 (show nope))

testParseShowInverse :: (Show a, Eq a) => TestName -> Text -> (Text -> Either String a) -> TestTree
testParseShowInverse testName input parser = testCase testName test
  where
    unsafeTestParse x = let Right r = parser x in r
    expected = unsafeTestParse input
    actual = unsafeTestParse (showT (unsafeTestParse input))
    test = assertEqual
        "parse input ≠ (parse.show.parse) input"
        expected
        actual

nat :: Int -> N.Expr
nat n = N.EAbs (Var "f")
               (N.EAbs (Var "x")
                       (iterate (N.EApp (N.EVar (Var "f"))) (N.EVar (Var "x")) !! n))

showT :: Show a => a -> Text
showT = T.pack . show
