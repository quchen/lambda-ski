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
                            evaluated = eval deBruijn
                            nominalAgain = deBruijnToNominal evaluated

                        outputStrLn "Nominal input"
                        outputStrLn "============="
                        outputStrLn ((toString . N.prettyAnsi) nominal)
                        outputStrLn ""
                        outputStrLn "→ De Bruijn"
                        outputStrLn "==========="
                        outputStrLn ((toString . B.prettyAnsi) deBruijn)
                        outputStrLn ""
                        outputStrLn "SKI version"
                        outputStrLn "==========="
                        outputStrLn ((toString . S.prettyAnsi) (unsafeNominalToSki nominal))
                        outputStrLn ""
                        outputStrLn "⇝ De Bruijn"
                        outputStrLn "==========="
                        outputStrLn ((toString . B.prettyAnsi) evaluated)
                        outputStrLn ""
                        outputStrLn "→ Nominal outout"
                        outputStrLn "================"
                        outputStrLn ((toString . N.prettyAnsi) nominalAgain)

                        loop )

toString :: Doc AnsiStyle -> String
toString = T.unpack . renderStrict . layoutPretty defaultLayoutOptions

{-
(λ +1 -1 true false Y 1. (λ =0 + -. (λ <= 2. Y (λrec n. (<= n 1) n (+ (rec (- n 1)) (rec (- n 2)))) ) (λm n. =0 (- m n)) (+ 1 1) ) (λn. n (λ_. false) true) (λm n. m +1 n) (λm n. n -1 m) ) (λn f x. f (n f x)) (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) (λt _. t) (λ_ f. f) (λf. (λx. f (x x)) (λx. f (x x))) (λf x. f x) (λf x. f (f (f (f (f (x))))))
-}
