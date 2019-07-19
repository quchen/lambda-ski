{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where



import           Data.Maybe
import           Data.List
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Test.Tasty
import           Test.Tasty.HUnit      hiding (assertEqual)
import qualified Test.Tasty.HUnit      as HUnit
import           Test.Tasty.QuickCheck

import Convert
import DeBruijn        as B
import ExamplePrograms
import QuasiQuoter
import Marshal
import Nominal         as N
import Ski             as S



main :: IO ()
main = defaultMain tests

newtype Expected a = Expected a
newtype Actual a = Actual a

-- | Type-safe version of 'HUnit.assertEqual'
assertEqual :: (Eq a, Show a) => Maybe String -> Actual a -> Expected a -> Assertion
assertEqual description (Actual actual) (Expected expected)
  = HUnit.assertEqual (fromMaybe "" description) expected actual

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
                [ testParseShowInverse "Y combinator" "Y = S S K (S (K (S S (S (S S K)))) K)" S.parse
                , testParseShowInverse "factorial" (showT (nominalToSki True factorial)) S.parse
                , testParseShowInverse "fibonacci" (showT (nominalToSki True fibonacci)) S.parse
                , testParseShowInverse "Hello World" (showT (nominalToSki True helloWorld)) S.parse
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
                [nominal|
                    (λ+1 1 2.
                        (λ+.
                            + 2 1
                        )
                        (λm n. m +1 n)
                    )
                    (λn f x. f (n f x))
                    (λf x. f x)
                    (λf x. f (f x))
                |]
                (toNominal (3 :: Int))
            , let n = 5 :: Int
                  fac k = product [1..k]
              in testReduceNominalViaDeBruijn
                (Just ("factorial(" ++ show n ++ ")"))
                (N.EApp factorial (toNominal n))
                (toNominal (fac n))
            , let n = 8 :: Int
                  fibs = (0 :: Int) : 1 : zipWith (+) fibs (tail fibs)
                  fib k = fibs !! k
              in testReduceNominalViaDeBruijn
                (Just ("fibonacci(" ++ show n ++ ")"))
                (N.EApp fibonacci (toNominal n))
                (toNominal (fib n))
            ]
        , localOption (QuickCheckMaxSize 10) $ testGroup "Marshalling"
            [ testMarshalling "()" (arbitrary :: Gen ())
            , testMarshalling "Bool" (arbitrary :: Gen Bool)
            , testMarshalling "Int" (fmap getNonNegative arbitrary :: Gen Int)
            , testMarshalling "Integer" (fmap getNonNegative arbitrary :: Gen Integer)
            , testMarshalling "Maybe Int" (oneof
                [ pure (Nothing :: Maybe Int)
                , fmap (Just . getNonNegative) arbitrary
                ])
            , testMarshalling "Either Bool Int" (oneof
                [ fmap Left arbitrary
                , fmap (Right . getNonNegative) arbitrary
                ] :: Gen (Either Bool Int))
            , testMarshalling "[Int]" (listOf (fmap getNonNegative arbitrary) :: Gen [Int])
            , testMarshalling "(Int, Bool)" (do
                NonNegative n <- arbitrary
                b <- arbitrary
                pure (n,b) :: Gen (Int, Bool))
            , testMarshalling "(Int, Bool, Int)" (do
                NonNegative n <- arbitrary
                b <- arbitrary
                NonNegative n' <- arbitrary
                pure (n,b,n') :: Gen (Int, Bool,Int))
            ]
        , localOption (Timeout 1000000 "1 second") $ testGroup "stdlib"
            [ testStdlib (Just "let") "let (λ x. x) (λ identity. identity x)" (FreeVar "x")
            , testGroup "functions"
                [ testStdlib Nothing "id x" (FreeVar "x")
                , testStdlib Nothing "const x y" (FreeVar "x")
                ]
            , testGroup "Booleans"
                [ testStdlib Nothing "True x y" (FreeVar "x")
                , testStdlib Nothing "False x y" (FreeVar "y")
                , testStdlib Nothing "not False" True
                , testStdlib Nothing "not True" False
                , testStdlib Nothing "and True False" False
                , testStdlib Nothing "and True True" True
                , testStdlib Nothing "or False True" True
                , testStdlib Nothing "or False False" False
                ]
            , testGroup "Naturals"
                [ testGroup "Arithmetic"
                    [ testStdlib Nothing "+ 1 2" (3 :: Int)
                    , testStdlib Nothing "- 3 2" (1 :: Int)
                    , testStdlib Nothing "pred 3" (2 :: Int)
                    , testStdlib Nothing "succ 2" (3 :: Int)
                    , testStdlib Nothing "* 2 3" (6 :: Int)
                    , testStdlib Nothing "^ 2 3" (8 :: Int)
                    ]
                , testGroup "Numbers"
                    [ testGroup "Digits"
                        [ testStdlib Nothing (N.unsafeParse (showT n)) n | n <- [0..9::Int] ]
                    ]
                , testGroup "Eq/Ord"
                    [ testStdlib Nothing "== 1 1" True
                    , testStdlib Nothing "!= 1 2" True
                    , testStdlib Nothing "< 1 2" True
                    , testStdlib Nothing "> 1 2" False
                    , testStdlib Nothing "<= 1 2" True
                    , testStdlib Nothing ">= 1 2" False
                    ]
                , testStdlib Nothing "even 2" (even (2::Int))
                , testStdlib Nothing "odd 2" (odd (2::Int))
                ]
            , testGroup "Pairs"
                [ testStdlib Nothing "fst (pair a b)" (FreeVar "a")
                , testStdlib Nothing "snd (pair a b)" (FreeVar "b")
                ]
            , testGroup "Lists"
                [ testGroup "null"
                    [ testStdlib Nothing "null []" True
                    , testStdlib Nothing "null (: a b)" False
                    ]
                , testStdlib Nothing "head (: a b)" (FreeVar "a")
                , testStdlib Nothing "tail (: a b)" (FreeVar "b")
                , testStdlib (Just "map (1+) [1,2]") "map (+ 1) (: 1 (: 2 []))" [2, 3 :: Int]
                , testStdlib (Just "[1] ++ [2,3]") "++ (: 1 []) (: 2 (: 3 []))" [1, 2, 3 :: Int]
                , testGroup "zip"
                    [ testStdlib (Just "zipWith f [] ys")
                        "zipWith f [] ys"
                        ([] :: [Int])
                    , testStdlib (Just "zipWith const [1] [x]")
                        "zipWith const (: 1 []) (: x [])"
                        [1 :: Int]
                    , testStdlib
                        (Just "zipWith (+) [1,2,3] [3,2,1]")
                        "zipWith + (: 1 (: 2 (: 3 []))) (: 3 (: 2 (: 1 [])))"
                        (zipWith (+) [1,2,3] [3,2,1 :: Int])
                    ]
                , testStdlib Nothing "head (repeat a)" (FreeVar "a")
                , testGroup "index"
                    [ testStdlib Nothing "index 3 (repeat a)" (FreeVar "a")
                    , testStdlib Nothing "index 3 (iterate (+ 1) 0)" (3 :: Int)
                    ]
                , testGroup "take"
                    [ testStdlib (Just "take 2 []")      "take 2 []"                   ([] :: [()])
                    , testStdlib (Just "take 2 [0,1,2]") "take 2 (: 0 (: 1 (: 2 [])))" [0, 1 :: Int]
                    ]
                , testGroup "drop"
                    [ testStdlib (Just "drop 2 []")      "drop 2 []"                   ([] :: [()])
                    , testStdlib (Just "drop 2 [0]")     "drop 2 (: 0 [])"             ([] :: [()])
                    , testStdlib (Just "drop 2 [0,1,2]") "drop 2 (: 0 (: 1 (: 2 [])))" [2 :: Int]
                    ]
                , testGroup "splitAt"
                    [ testStdlib (Just "splitAt 2 []")      "splitAt 2 []"                   (splitAt 2 ([] :: [()]))
                    , testStdlib (Just "splitAt 0 [0]")     "splitAt 0 (: 0 [])"             (splitAt 0 [0::Int])
                    , testStdlib (Just "splitAt 2 [0]")     "splitAt 2 (: 0 [])"             (splitAt 2 [0::Int])
                    , testStdlib (Just "splitAt 2 [0,1,2]") "splitAt 2 (: 0 (: 1 (: 2 [])))" (splitAt 2 [0,1,2::Int])
                    ]
                , testStdlib Nothing "filter (!= 1) (: 0 (: 1 (: 2 [])))" (filter (/= 1) [0,1,2 :: Int])
                , testStdlib (Just "takeWhile (3 <= x) [0..]") "takeWhile (λx. <= x 3) (iterate (+ 1) 0)" (takeWhile (<= 3) [0::Int ..])
                , testStdlib (Just "partition (!= 1) [1,2,1,3]") "partition (!= 1) (: 1 (:  2 (: 1 (: 3 []))))" (partition (/= 1) [1,2,1,3::Int])
                ]
            ]
        ]
        , testGroup "SKICB ⇝ SKICB"
            [ testGroup "Individual combinators"
                [ testReduceSki Nothing "I x"     "x"
                , testReduceSki Nothing "K x y"   "x"
                , testReduceSki Nothing "S f g x" "f x (g x)"
                , testReduceSki Nothing "C f y x" "f x y"
                , testReduceSki Nothing "B f g x" "f (g x)"
                ]
            , testGroup "Examples from Wikipedia"
                [ testReduceSki Nothing "C I x y" "y x"
                , testReduceSki Nothing "S (K (S I)) (S (K K) I) x y" "y x"
                ]
            ]
        , testGroup "Nominal → SKICB ⇝ SKICB"
            [ testReduceSkiViaNominal
                Nothing
                "(λx. x) ok"
                "ok"
            , testReduceSkiViaNominal
                (Just "Y (const ok)")
                "(λf. (λx. f (x x)) (λx. f (x x))) (λ_. ok)"
                "ok"
            , testReduceSkiViaNominal
                (Just "2 + 1")
                [nominal|
                    (λ+1 1 2.
                        (λ+.
                            + 2 1
                        )
                        (λm n. m +1 n)
                    )
                    (λn f x. f (n f x))
                    (λf x. f x)
                    (λf x. f (f x))
                |]
                (toNominal (3 :: Int))
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
    test = assertEqual Nothing (Actual (B.unsafeParse input)) (Expected expected)

testNominalToDeBruijn :: N.Expr -> B.Expr -> TestTree
testNominalToDeBruijn input expected = testCase testName test
  where
    testName = show input
    actual = nominalToDeBruijn input
    test = assertEqual Nothing (Actual actual) (Expected expected)

testNominalToDeBruijnAndBack :: Maybe TestName -> N.Expr -> TestTree
testNominalToDeBruijnAndBack mTestName source = testCase testName test
  where
    testName = fromMaybe (show source) mTestName
    actual = (Actual . deBruijnToNominal . nominalToDeBruijn) source
    test = assertEqual Nothing actual (Expected source)

testReduceDeBruijn :: Maybe TestName -> B.Expr -> B.Expr -> TestTree
testReduceDeBruijn mTestName input expected = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = Actual (evalTo B.normalForm input)
    test = assertEqual Nothing actual (Expected expected)

testReduceNominalViaDeBruijn :: Maybe TestName -> N.Expr -> N.Expr -> TestTree
testReduceNominalViaDeBruijn mTestName input expected = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = (Actual . deBruijnToNominal . evalTo B.normalForm . nominalToDeBruijn) input
    test = assertEqual Nothing actual (Expected expected)

testStdlib :: ToNominal a => Maybe TestName -> N.Expr -> a -> TestTree
testStdlib mTestName input expected = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    eval = evalTo B.normalForm . nominalToDeBruijn . stdlib
    actual = Actual (eval input)
    expected' = Expected (eval (toNominal expected))
    test = assertEqual Nothing actual expected'

testReduceSki :: Maybe TestName -> S.Expr -> S.Expr -> TestTree
testReduceSki mTestName input expected = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = Actual (S.normalForm input)
    test = assertEqual (Just (show input ++ " ⇝")) actual (Expected expected)

testReduceSkiViaNominal :: Maybe TestName -> N.Expr -> N.Expr -> TestTree
testReduceSkiViaNominal mTestName input expectedNominal = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = Actual (S.normalForm (nominalToSki True input))
    expected = Expected (nominalToSki True expectedNominal)
    test = assertEqual Nothing actual expected

testHelloWorldNominal :: TestTree
testHelloWorldNominal = testCase "Lambda calculus version, old implementation" test
  where
    test = assertEqual Nothing actual expected
    expected = Expected "Hello, world!\n"
    actual = (Actual . marshal . deBruijnToNominal . evalTo B.normalForm . nominalToDeBruijn) helloWorld

    marshal :: N.Expr -> String
    marshal (N.EAbs _ e) = marshal e
    marshal (N.EApp (N.EApp (N.EVar (Var "extern_outChr")) increments) cont)
      = let char (N.EApp (N.EVar (Var "extern_succ")) rest) = succ (char rest)
            char (N.EVar (Var "extern_0")) = minBound
            char nope = error ("Bad increment: " ++ previewError (show nope))
        in char increments : marshal cont
    marshal (N.EVar (Var "extern_eof")) = ""
    marshal nope = error ("Marshalling broken or bad λAST: " ++ previewError (show nope))

testHelloWorldSki :: TestTree
testHelloWorldSki = testCase "SKI calculus" test
  where
    test = assertEqual Nothing actual expected
    expected = Expected "Hello, world!\n"
    actual = (Actual . marshal . S.normalForm . nominalToSki True) helloWorld

    marshal :: S.Expr -> String
    marshal (S.EApp (S.EApp (S.EFree "extern_outChr") increments) cont)
      = let char (S.EApp (S.EFree "extern_succ") rest) = succ (char rest)
            char (S.EFree "extern_0") = minBound
            char nope = error ("Bad increment: " ++ previewError (show nope))
        in char increments : marshal cont
    marshal (S.EFree "extern_eof") = ""
    marshal nope = error ("Cannot marshal value: " ++ previewError (show nope))

testParseShowInverse :: (Show a, Eq a) => TestName -> Text -> (Text -> Either String a) -> TestTree
testParseShowInverse testName input parser = testCase testName test
  where
    unsafeTestParse x = let Right r = parser x in r
    expected = unsafeTestParse input
    actual = unsafeTestParse (showT (unsafeTestParse input))
    test = assertEqual
        (Just "parse input ≠ (parse.show.parse) input")
        (Actual actual)
        (Expected expected)

testMarshalling
    :: (Arbitrary a, Show a, Eq a, FromDeBruijn a, ToNominal a)
    => TestName
    -> Gen a
    -> TestTree
testMarshalling testName gen = testProperty testName test
  where
    test = forAll gen (\x -> (fromDeBruijn . nominalToDeBruijn . toNominal) x == Just x)

previewError :: String -> String
previewError = dotdot 256 . T.unpack . T.unwords . map T.strip . T.lines . T.pack
  where
    dotdot :: Int -> String -> String
    dotdot _ []     = ""
    dotdot 0 _      = "…"
    dotdot n (x:xs) = x : dotdot (n-1) xs

showT :: Show a => a -> Text
showT = T.pack . show
