module Main where



import Data.Char

import           Convert
import qualified ExamplePrograms as Example



main :: IO ()
main = do
    let program = Example.helloWorld
        programSki = nominalToSki program
    putStrLn "module Main (main) where"
    putStrLn ""
    putStrLn "main = hello (\\char cont -> putChar char >> cont) succ minBound (pure ())"
    putStrLn "s x y z = x z (y z)"
    putStrLn "k x y = x"
    putStrLn "i = s k k"
    putStrLn "b = s (k s) k"
    putStrLn "c = s (s (k (s (k s) k)) s) (k k)"
    putStrLn ""
    putStrLn "hello :: (char -> io -> io) -> (char -> char) -> char -> io -> io"
    putStrLn ("hello = " ++ map toLower (show programSki))
