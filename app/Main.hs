module Main where

import Lib

main :: IO ()
main = putStrLn . prettyLambda . enormousize 4 $ ycL
