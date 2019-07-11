{-# LANGUAGE OverloadedStrings #-}

module Main where



import           Control.Monad
import           Data.Char
import           qualified Data.Map as M
import           Data.List                             hiding (group)
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Environment

import           Convert
import qualified ExamplePrograms as Example
import           Nominal         as N
import           Ski             as S



main :: IO ()
main = do
    args <- getArgs
    case args of
        [lang] -> case M.lookup lang backends of
            Just code -> let render = renderStrict . layoutPretty defaultLayoutOptions
                         in T.putStrLn (render code)
            Nothing -> error ("Unsupported language: " ++ lang ++ ". Options: " ++ intercalate ", " (M.keys backends))
        [] -> error ("No target language specified. Options: " ++ intercalate ", " (M.keys backends))
        _ -> error "Expecting single parameter (backend language)"

backends :: M.Map String (Doc ann)
backends = M.fromList
    [ ("python3",    python    )
    , ("javascript", javascript)
    , ("ruby",       ruby      )
    , ("haskell",    haskell   )
    , ("haskell2",    haskell2   )]

allowICB :: Bool
allowICB = False

helloSki :: S.Expr
helloSki = nominalToSki allowICB
    (N.EAbs (N.Var "extern_outChr")
     (N.EAbs (N.Var "extern_eof")
      (N.EAbs (N.Var "extern_succ")
       (N.EAbs (N.Var "extern_0")
        Example.helloWorld))))

haskell :: Doc ann
haskell = vcat
    [ "module Main (main) where"
    , "-- Does not typecheck yet :-("
    , "main = putStr (hello (:) [] succ minBound)"
    , "hello :: (char -> io -> io) -> io -> (char -> char) -> char -> io"
    , hang 4 ("hello = " <> skiToHs False helloSki)
    ]
  where
    skiToHs :: Bool -> S.Expr -> Doc ann
    skiToHs _ S = "(<*>)"
    skiToHs _ K = "pure"
    skiToHs _ I = "id"
    skiToHs _ B = "(.)"
    skiToHs _ C = "flip"
    skiToHs _ (EFree name) = pretty name
    skiToHs True app@S.EApp{} = "(" <> group line' <> skiToHs False app <> group line' <> ")"
    skiToHs False (S.EApp e1 e2)
      = let (hd, args) = collectArgs e1 e2
        in concatWith (\x y -> x <> group line <> y) (skiToHs False hd : map (skiToHs True) args)
      where
        collectArgs :: S.Expr -> S.Expr -> (S.Expr, [S.Expr])
        collectArgs (S.EApp e e') arg
          = let (hd, args) = collectArgs e e'
            in (hd, args ++ [arg])
        collectArgs hd arg = (hd, [arg])

haskell2 :: Doc ann
haskell2 = vcat
    [ "module Main (main) where"
    , ""
    , "main = (putStrLn . marshal . nf) hello"
    , ""
    , "data SK"
    , "    = S"
    , "    | K"
    , "    | I"
    , "    | B"
    , "    | C"
    , "    | EFree String"
    , "    | EApp SK SK"
    , ""
    , "nf (EApp e x) = case nf e of"
    , "    EApp K y          -> nf y"
    , "    EApp (EApp S f) g -> nf (EApp (EApp f x) (EApp g x))"
    , "    I                 -> nf x"
    , "    EApp (EApp B f) g -> nf (EApp f (EApp g x))"
    , "    EApp (EApp C f) y -> nf (EApp (EApp f x) y)"
    , "    other             -> EApp other (nf x)"
    , "nf x = x"
    , ""
    , "marshal (EApp (EApp (EFree \"extern_outChr\") increments) cont)"
    , "  = let char (EApp (EFree \"extern_succ\") rest) = succ (char rest)"
    , "        char (EFree \"extern_0\") = minBound"
    , "    in char increments : marshal cont"
    , "marshal (EFree \"extern_eof\") = \"\""
    , ""
    , "hello = " <> skiToHs helloSki
    ]
  where
    skiToHs :: S.Expr -> Doc an
    skiToHs S = "S"
    skiToHs K = "K"
    skiToHs I = "I"
    skiToHs B = "B"
    skiToHs C = "C"
    skiToHs (EFree name) = parens ("EFree" <+> pretty name)
    skiToHs (S.EApp e1 e2) = "EApp" <+> parens (skiToHs e1) <+> parens (skiToHs e2)

python :: Doc ann
python = vcat
    [ "#!/usr/bin/env python3"
    , ""
    ,"S = lambda f: lambda g: lambda x: f(x)(g(x))"
    , "K = lambda x: lambda _: x"
    , if allowICB
        then vcat
            [ "I = " <> snd (compile "S K K")
            , "B = " <> snd (compile "S (K S) K")
            , "C = " <> snd (compile "S (S (K (S (K S) K)) S) (K K)")
            ]
        else ""
    , let (floats, trunk) = compile helloSki
      in vcat (concat
        [ if not (null floats)
            then [ "# Python’s parser supports only 100 levels of nesting in expressions,"
                 , "# which we hack around by floating out deeply nested subexpressions." ]
            else []
        , floats
        , [hang 4 ("hello = " <> trunk)]
        ])
    , ""
    , "print(hello (lambda ascii: lambda rest: chr(ascii) + rest) ('') (lambda x: x+1) (0), end='')"
    ]
  where
    compile :: S.Expr -> ([Doc ann], Doc ann)
    compile source
      = let WS result = compile' 0 source
            (floats, _state, trunk) = result 0
        in (floats, trunk)

    compile' :: Int -> S.Expr -> WS [Doc ann] Int (Doc ann)
    compile' _ S = pure "S"
    compile' _ K = pure "K"
    compile' _ I = pure "I"
    compile' _ B = pure "B"
    compile' _ C = pure "C"
    compile' _ (EFree name) = pure (pretty name)
    compile' nestingDepth (S.EApp f x) = do
        hd <- compile' nestingDepth f
        let argDepth = exprDepth x
        tl <- if nestingDepth < 64 || argDepth < 16
            then compile' (nestingDepth+1) x
            else do
                tl_floatedOutName <- do
                    uniqueIndex <- get
                    put (uniqueIndex+1)
                    pure ("hello_" <> pretty uniqueIndex)
                floatBody <- compile' 0 x
                let floatDefinition = hang 4 (tl_floatedOutName <> " = " <> floatBody)
                tell [floatDefinition]
                pure tl_floatedOutName
        pure (hd <> group line' <> "(" <> group line' <> tl <> group line' <> ")")

    exprDepth :: S.Expr -> Int
    exprDepth (S.EApp f x) = max (exprDepth f) (1 + exprDepth x)
    exprDepth _ = 0

newtype WS w s a = WS (s -> (w, s, a))

instance Functor (WS w s) where
    fmap f (WS mx) = WS (\s -> let (w', s', x') = mx s in (w', s', f x'))

instance Monoid w => Applicative (WS w s) where
    pure x = WS (\s -> (mempty, s, x))
    (<*>) = ap

instance Monoid w => Monad (WS w s) where
    WS mx >>= f = WS (\s ->
        let (w', s', x') = mx s
            (w'', s'', x'') = let WS mx' = f x' in mx' s'
        in (w' <> w'', s'', x''))

get :: Monoid w => WS w s s
get = WS (\s -> (mempty, s, s))

put :: Monoid w => s -> WS w s ()
put x = WS (\_ -> (mempty, x, ()))

tell :: w -> WS w s ()
tell x = WS (\s -> (x, s, ()))

javascript :: Doc ann
javascript = vcat
    [ "#!/usr/bin/env node"
    , ""
    ,"S = f => g => x => f(x)(g(x));"
    , "K = x => _ => x;"
    , if allowICB
        then vcat
            [ "I = " <> skiToJs "S K K" <> ";"
            , "B = " <> skiToJs "S (K S) K" <> ";"
            , "C = " <> skiToJs "S (S (K (S (K S) K)) S) (K K)" <> ";"
            ]
        else ""
    , hang 4 ("hello = " <> skiToJs helloSki <> ";")
    , ""
    , "process.stdout.write(hello (asc => rest => String.fromCharCode(asc) + rest) ('') (x => x+1) (0));"
    ]
  where
    skiToJs :: S.Expr -> Doc ann
    skiToJs S = "S"
    skiToJs K = "K"
    skiToJs I = "I"
    skiToJs B = "B"
    skiToJs C = "C"
    skiToJs (EFree name) = pretty name
    skiToJs (S.EApp f x) = skiToJs f <> group line' <> "(" <> group line' <> skiToJs x <> group line' <> ")"

ruby :: Doc ann
ruby = vcat
    [ "#!/usr/bin/env ruby"
    , ""
    , "S = lambda { |f| lambda { |g| lambda { |x| f.call(x).call(g.call(x)) } } }"
    , "K = lambda { |x| lambda { |_| x } }"
    , if allowICB
        then vcat
            [ "I = " <> skiToRuby "S K K"
            , "B = " <> skiToRuby "S (K S) K"
            , "C = " <> skiToRuby "S (S (K (S (K S) K)) S) (K K)"
            ]
        else ""
    , hang 4 ("hello = " <> skiToRuby helloSki)
    , ""
    , "puts(hello.call(lambda { |asc| lambda { |rest| asc.chr + rest } }).call('').call(lambda {|x| x+1}).call(0))"
    ]
  where
    skiToRuby :: S.Expr -> Doc ann
    skiToRuby S = "S"
    skiToRuby K = "K"
    skiToRuby I = "I"
    skiToRuby B = "B"
    skiToRuby C = "C"
    skiToRuby (EFree name) = pretty name
    skiToRuby (S.EApp f x) = skiToRuby f <> ".call(" <> group line' <> skiToRuby x <> group line' <> ")"
