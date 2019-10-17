{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main (main) where



import           Data.Maybe
import           Data.List
import           Control.Applicative
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
        , localOption (QuickCheckMaxSize 10) $ localOption (Timeout 10000000 "10 seconds") $ testGroup "stdlib"
            [ testStdlib (Just "let") "let (λ x. x) (λ identity. identity x)" (FreeVar "x")
            , testGroup "functions"
                [ testStdlib Nothing "id x" (FreeVar "x")
                , testStdlib Nothing "const x y" (FreeVar "x")
                ]
            , testGroup "Booleans"
                [ testStdlib Nothing "True x y"  (FreeVar "x")
                , testStdlib Nothing "False x y" (FreeVar "y")
                , testStdlibQC "not" "not"       not            arbitrary
                , testStdlibQC "and" "λx. x and" (uncurry (&&)) arbitrary
                , testStdlibQC "or" "λx. x or"   (uncurry (||)) arbitrary
                ]
            , testGroup "Naturals"
                [ testGroup "Arithmetic"
                    [ testStdlibQC "+"    "λx. x +" (uncurry (+))           (liftA2 (,) nat nat)
                    , testStdlibQC "-"    "λx. x -" (\(a,b) -> max 0 (a-b)) (liftA2 (,) nat nat)
                    , testStdlibQC "succ" "succ"    succ                    nat
                    , testStdlibQC "pred" "pred"    (\x -> max 0 (pred x))  nat
                    , testStdlibQC "*"    "λx. x *" (uncurry (*))           (liftA2 (,) nat nat)
                    , testStdlibQC
                        "^"
                        "λx. x ^"
                        (uncurry (^))
                        (resize 3 (do (Positive b, Positive e) <- arbitrary; pure ((b,e) :: (Int, Int))))
                    ]
                , testGroup "Numbers"
                    [ testGroup "Digits"
                        [ testStdlib Nothing (N.unsafeParse (showT n)) n | n <- [0..9::Int] ]
                    ]
                , testGroup "Eq/Ord"
                    [ testStdlibQC "==" "λx. x ==" (uncurry (==)) (liftA2 (,) nat nat)
                    , testStdlibQC "!=" "λx. x !=" (uncurry (/=)) (liftA2 (,) nat nat)
                    , testStdlibQC "<"  "λx. x <"  (uncurry (<))  (liftA2 (,) nat nat)
                    , testStdlibQC ">"  "λx. x >"  (uncurry (>))  (liftA2 (,) nat nat)
                    , testStdlibQC "<=" "λx. x <=" (uncurry (<=)) (liftA2 (,) nat nat)
                    , testStdlibQC ">=" "λx. x >=" (uncurry (>=)) (liftA2 (,) nat nat)
                    ]
                , testStdlibQC "even" "even" even nat
                , testStdlibQC "odd"  "odd"  odd  nat
                ]
            , testGroup "Pairs"
                [ testStdlib Nothing "fst (pair a b)" (FreeVar "a")
                , testStdlib Nothing "snd (pair a b)" (FreeVar "b")
                ]
            , testGroup "Either"
                [ testStdlib Nothing "isLeft (Left x)" True
                , testStdlib Nothing "isLeft (Right x)" False
                , testStdlib Nothing "fromLeft (Left x)" (FreeVar "x")
                , testStdlib Nothing "isRight (Left x)" False
                , testStdlib Nothing "isRight (Right x)" True
                , testStdlib Nothing "fromRight (Right x)" (FreeVar "x")
                ]
            , testGroup "Maybe"
                [ testStdlib Nothing "isNothing Nothing" True
                , testStdlib Nothing "isJust (Just x)" True
                , testStdlib Nothing "isJust Nothing" False
                , testStdlib Nothing "fromJust (Just x)" (FreeVar "x")
                ]
            , testGroup "Lists"
                [ testStdlibQC "null" "null" null (listOf nat)
                , testStdlib Nothing "head (: a b)" (FreeVar "a")
                , testStdlib Nothing "tail (: a b)" (FreeVar "b")
                , testStdlib (Just "map (1+) [1,2]") "map (+ 1) (: 1 (: 2 []))" [2, 3 :: Int]
                , testStdlib (Just "mapMaybe id [Just 1, Nothing, Just 2]") "mapMaybe id (: (Just 1) (: Nothing (: (Just 3) [])))" [1, 3 :: Int]
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
                , testStdlib (Just "filter (/= 1) [0,1,2]") "filter (!= 1) (: 0 (: 1 (: 2 [])))" (filter (/= 1) [0,1,2 :: Int])
                , testStdlib (Just "takeWhile (3 <= x) [0..]") "takeWhile (λx. <= x 3) (iterate (+ 1) 0)" (takeWhile (<= 3) [0::Int ..])
                , testStdlib (Just "take 3 (dropWhile (3 <= x) [0..])") "take 3 (dropWhile (λx. <= x 3) (iterate (+ 1) 0))" (take 3 (dropWhile (<= 3) [0::Int ..]))
                , testStdlib (Just "partition (<= 2) [1,2,1,3]") "partition (λx. <= x 2) (: 1 (:  2 (: 1 (: 3 []))))" (partition (<= 2) [1,2,1,3::Int])
                , testStdlib (Just "unzip [(1,2), (2,3), (3,4)]") "unzip (: (pair 1 2) (: (pair 2 3) (: (pair 3 4) [])))" (unzip [(1,2), (2,3), (3::Int,4::Int)])
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
                (Just "1")
                (stdlib [nominal| 1 |])
                (toNominal (1 :: Int))
            , testReduceSkiViaNominal
                (Just "2 + 1")
                (stdlib [nominal| + 2 1 |])
                (toNominal (2 + 1 :: Int))
            ]
        , testGroup "Hello, world!"
            [ testHelloWorldNominal
            , testHelloWorldSki
            ]
    ]

nat :: Gen Int
nat = do
    NonNegative x <- arbitrary
    pure x

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
    actual = Actual (B.evalTo B.normalForm input)
    test = assertEqual Nothing actual (Expected expected)

testReduceNominalViaDeBruijn :: Maybe TestName -> N.Expr -> N.Expr -> TestTree
testReduceNominalViaDeBruijn mTestName input expected = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = (Actual . deBruijnToNominal . B.evalTo B.normalForm . nominalToDeBruijn) input
    test = assertEqual Nothing actual (Expected expected)

testStdlib :: ToNominal a => Maybe TestName -> N.Expr -> a -> TestTree
testStdlib mTestName input expected = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    eval = B.evalTo B.normalForm . nominalToDeBruijn . stdlib
    actual = Actual (eval input)
    expected' = Expected (eval (toNominal expected))
    test = assertEqual Nothing actual expected'

testStdlibQC
    :: (Show input, Arbitrary input, Eq output, FromDeBruijn output, ToNominal input)
    => TestName
    -> N.Expr
    -> (input -> output)
    -> Gen input
    -> TestTree
testStdlibQC testName lcInput haskellReferenceImplementation gen = testProperty testName test
  where
    eval = B.evalTo B.normalForm . nominalToDeBruijn . stdlib
    test = forAllShrink gen shrink $ \x ->
        let actual = fromDeBruijn (eval (N.EApp lcInput (toNominal x)))
            expected = Just (haskellReferenceImplementation x)
        in actual == expected

testReduceSki :: Maybe TestName -> S.Expr -> S.Expr -> TestTree
testReduceSki mTestName input expected = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = Actual (S.evalTo S.normalForm input)
    test = assertEqual (Just (show input ++ " ⇝")) actual (Expected expected)

testReduceSkiViaNominal :: Maybe TestName -> N.Expr -> N.Expr -> TestTree
testReduceSkiViaNominal mTestName input expectedNominal = testCase testName test
  where
    testName = fromMaybe (show input) mTestName
    actual = Actual (S.evalTo S.normalForm (nominalToSki True input))
    expected = Expected (S.evalTo S.normalForm (nominalToSki True expectedNominal))
    test = assertEqual Nothing actual expected

testHelloWorldNominal :: TestTree
testHelloWorldNominal = testCase "Lambda calculus version, old implementation" test
  where
    test = assertEqual Nothing actual expected
    expected = Expected "Hello, world!\n"
    actual = (Actual . marshal . deBruijnToNominal . B.evalTo B.normalForm . nominalToDeBruijn) helloWorld

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
    actual = (Actual . marshal . S.evalTo S.normalForm . nominalToSki True) helloWorld

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
    test = forAllShrink gen shrink (\x -> (fromDeBruijn . B.evalTo B.normalForm . nominalToDeBruijn . toNominal) x == Just x)

previewError :: String -> String
previewError = dotdot 256 . T.unpack . T.unwords . map T.strip . T.lines . T.pack
  where
    dotdot :: Int -> String -> String
    dotdot _ []     = ""
    dotdot 0 _      = "…"
    dotdot n (x:xs) = x : dotdot (n-1) xs

showT :: Show a => a -> Text
showT = T.pack . show
