module Main where



import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           System.Console.Haskeline


import Convert
import DeBruijn as B
import Nominal  as N
import Ski      as S



main :: IO ()
main = runInputT defaultSettings (withInterrupt loop)
  where
    loop :: InputT IO ()
    loop = handle
        (\Interrupt -> outputStrLn "^C" >> loop)
        (do
            input <- getInputLine "> "
            case input of
                Nothing -> outputStrLn "^D"
                Just code -> case N.parse (T.pack code) of
                    Left err -> outputStrLn ("Parse error: " ++ show err) >> loop
                    Right nominal -> do
                        let deBruijn = nominalToDeBruijn nominal
                            evaluated = B.evalTo B.normalForm deBruijn
                            nominalAgain = deBruijnToNominal evaluated

                        outputStrLn "Nominal input"
                        outputStrLn "============="
                        outputStrLn ((toString . N.prettyAnsi) nominal)
                        outputStrLn ""
                        -- outputStrLn "→ De Bruijn"
                        -- outputStrLn "==========="
                        -- outputStrLn ((toString . B.prettyAnsi) deBruijn)
                        -- outputStrLn ""
                        -- outputStrLn "SKI version"
                        -- outputStrLn "==========="
                        -- outputStrLn ((toString . S.prettyAnsi) (nominalToSki True nominal))
                        -- outputStrLn ""
                        -- outputStrLn "⇝ SKI"
                        -- outputStrLn "==========="
                        -- outputStrLn ((toString . S.prettyAnsi) (S.evalTo S.normalForm (nominalToSki True nominal)))
                        -- outputStrLn ""
                        -- outputStrLn "⇝ De Bruijn"
                        -- outputStrLn "==========="
                        -- outputStrLn ((toString . B.prettyAnsi) evaluated)
                        -- outputStrLn ""
                        outputStrLn "→ Nominal outout"
                        outputStrLn "================"
                        outputStrLn ((toString . N.prettyAnsi) nominalAgain)

                        loop )

toString :: Doc AnsiStyle -> String
toString = T.unpack . renderStrict . layoutPretty defaultLayoutOptions

{-
    (\let.
        let (\x _. x)                                   (\ true.
        let (\_ y. y)                                   (\ false.
        let (\x. x)                                     (\ ifThenElse.
        let (\f. (\x. f (x x)) (\x. f (x x)))           (\ Y.
        let (\f x. f x)                                 (\ 1.
        let (\f x. f (f x))                             (\ 2.
        let (\n f x. n (\g h. h (g f)) (\_. x) (\u. u)) (\ pred.
        let (\n. n (\x. false) true)                    (\ =0.
        let (\m n f x. n f (m f x))                     (\ +.
        let (\m n. n pred m)                            (\ -.
        let (\m n. =0 (- m n))                          (\ <=.
            Y (\fib n. ifThenElse (<= n 2)
                       n
                       (+ (fib (- n 1))
                          (fib (- n 2)))))
        ))))))))))
    ) (\value body. body value)
(λf x. f (f (f (f (f (f (f x)))))))
-}
