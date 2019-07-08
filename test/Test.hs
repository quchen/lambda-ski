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
    [ let eVar ix = B.EVar ix (T.pack "<dummy>")
          eApp = B.EApp
          eAbs = B.EAbs
          eFree = B.EVarFree
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
        , testParseDeBruijn
            Nothing
            "λ (λ 1 0 (free 0))"
            (eAbs (eAbs (eApp (eApp (eVar 1) (eVar 0)) (eApp (eFree "free") (eVar 0)))))

        , testGroup "Source --parse--> Nominal"
            [ testParseShow "factorial, nominal" (T.pack (show factorial)) N.parse
            , testParseShow "fibonacci, nominal" (T.pack (show fibonacci)) N.parse
            , testParseShow "Hello World, nominal" (T.pack (show helloWorld)) N.parse
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
            , testNominalToDeBruijn "λok. (λx. x) ok" "(λ(λ0)0)"
            , testNominalToDeBruijn
                "λf. (λx. f (x x)) (λx. f (x x))"
                "(λ(λ1(0 0)) (λ1(0 0)))"
            ]
        , testGroup "Nominal → De Bruijn → Nominal"
            [ testNominalToDeBruijnAndBack Nothing (N.unsafeParse "λx y z.z y x")
            , testNominalToDeBruijnAndBack Nothing (N.unsafeParse "λ_ x.x")
            , testNominalToDeBruijnAndBack Nothing (N.unsafeParse "λ_. free")
            , testNominalToDeBruijnAndBack Nothing (N.unsafeParse "λbound. (λ_. free) bound free")
            , testNominalToDeBruijnAndBack (Just "Y") (N.unsafeParse "λf. (λx. f (x x)) (λx. f (x x))")
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
                (N.unsafeParse "(λx. x) (λok. ok)")
                "(λok. ok)"
            , testReduceNominalViaDeBruijn
                (Just "Y (const ok)")
                (N.unsafeParse "(λf. (λx. f (x x)) (λx. f (x x))) (λ_ ok. ok)")
                "λok. ok"
            , testReduceNominalViaDeBruijn
                (Just "2 + 1")
                (N.unsafeParse
                    " (λ+1 1 2.           \
                    \     (λ+.            \
                    \         + 2 1       \
                    \     )               \
                    \     (λm n. m +1 n)  \
                    \ )                   \
                    \ (λn f x. f (n f x)) \
                    \ (λf x. f x)         \
                    \ (λf x. f (f x))     ")
                "λf x. f (f (f x))"
            , testReduceNominalViaDeBruijn
                (Just "factorial(3)")
                (N.EApp factorial (N.unsafeParse "λf x. f (f (f x))"))
                "λf x. f (f (f (f (f (f x)))))"
            , let n = 8
              in testReduceNominalViaDeBruijn
                (Just ("fibonacci(" ++ show n ++ ")"))
                (N.EApp fibonacci (nat n))
                (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
                 in T.pack (show (nat (fibs !! n))))
            ]
        ]
        , testGroup "Nominal → SKICB ⇝ SKICB"
            [ testReduceSki
                Nothing
                "(λx. x) (λok. ok)"
                "(λok. ok)"
            , testReduceSki
                (Just "Y (const ok)")
                "(λf. (λx. f (x x)) (λx. f (x x))) (λ_ ok. ok)"
                "λok. ok"
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

testNominalToDeBruijn :: Text -> Text -> TestTree
testNominalToDeBruijn inputSrc expectedSrc = testCase testName test
  where
    testName = T.unpack inputSrc
    expected = B.unsafeParse expectedSrc
    actual = nominalToDeBruijn (N.unsafeParse inputSrc)
    test = assertEqual "" expected actual

testNominalToDeBruijnAndBack :: Maybe TestName -> N.Expr -> TestTree
testNominalToDeBruijnAndBack mTestName nominal = testCase testName test
  where
    testName = fromMaybe (show nominal) mTestName
    nominalAgain = deBruijnToNominal (nominalToDeBruijn nominal)
    test = assertEqual "" nominal nominalAgain

testReduceDeBruijn :: Maybe TestName -> Text -> Text -> TestTree
testReduceDeBruijn mTestName inputSrc expectedSrc = testCase testName test
  where
    testName = fromMaybe (T.unpack inputSrc) mTestName
    actual = eval (B.unsafeParse inputSrc)
    expected = B.unsafeParse expectedSrc
    test = assertEqual "" expected actual

testReduceNominalViaDeBruijn :: Maybe TestName -> N.Expr -> Text -> TestTree
testReduceNominalViaDeBruijn mTestName input expectedSrc = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = (deBruijnToNominal . eval . nominalToDeBruijn) input
    expected = N.unsafeParse expectedSrc
    test = assertEqual "" expected actual

testReduceSki :: Maybe TestName -> Text -> Text -> TestTree
testReduceSki mTestName nominalInputSrc expectedNominalSrc = testCase testName test
  where
    testName = fromMaybe (T.unpack nominalInputSrc) mTestName
    actual = S.normalForm (nominalToSki (N.unsafeParse nominalInputSrc))
    expected = nominalToSki (N.unsafeParse expectedNominalSrc)
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

testParseShow :: (Show a, Eq a) => TestName -> Text -> (Text -> Either String a) -> TestTree
testParseShow testName input parser = testCase testName test
  where
    unsafeTestParse x = let Right r = parser x in r
    expected = unsafeTestParse input
    actual = unsafeTestParse (T.pack (show (unsafeTestParse input)))
    test = assertEqual
        "parse input ≠ (parse.show.parse) input"
        expected
        actual

nat :: Int -> N.Expr
nat n = N.EAbs (Var "f")
               (N.EAbs (Var "x")
                       (iterate (N.EApp (N.EVar (Var "f"))) (N.EVar (Var "x")) !! n))
