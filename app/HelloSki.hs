{-# LANGUAGE OverloadedStrings #-}

module Main where



import           Control.Monad
import           Data.List                             hiding (group)
import qualified Data.Map                              as M
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
    , ("haskell",    haskell   )]

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
    [ "#!/usr/bin/env runhaskell"
    , ""
    , "module Main (main) where"
    , ""
    , "main = (putStr . marshal . nf) hello"
    , ""
    , "data SK = S | K | " <> (if allowICB then "I | B | C | " else "") <> "Free String | App SK SK"
    , ""
    , "nf (App e x) = case nf e of"
    , vsep (
        [ "    App K y         -> nf y"
        , "    App (App S f) g -> nf (App (App f x) (App g x))"]
        ++
        (if allowICB
            then
                [ "    I               -> nf x"
                , "    App (App B f) g -> nf (App f (App g x))"
                , "    App (App C f) y -> nf (App (App f x) y)"
                ]
            else [])
        ++
        ["    other           -> App other (nf x)"]
        )
    , "nf x = x"
    , ""
    , "marshal (App (App (Free \"extern_outChr\") increments) cont)"
    , "  = let char (App (Free \"extern_succ\") rest) = succ (char rest)"
    , "        char (Free \"extern_0\") = minBound"
    , "    in char increments : marshal cont"
    , "marshal (Free \"extern_eof\") = \"\""
    , ""
    , nest 4 ("hello = " <> skiToHs (nominalToSki allowICB Example.helloWorld))
    ]
  where
    skiToHs :: S.Expr -> Doc an
    skiToHs S = "S"
    skiToHs K = "K"
    skiToHs I = "I"
    skiToHs B = "B"
    skiToHs C = "C"
    skiToHs (EFree name) = parens ("Free" <+> dquotes (pretty name))
    skiToHs (S.EApp e1 e2) = "App" <> group line <> parens (skiToHs e1) <> group line <> parens (skiToHs e2)

-- Python has a maximum depth of 100 for its parser. The main difference of this
-- compiler compared to Javascript is that we have to keep track of nesting depth,
-- and extract subexpressions to circumvent this.
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
        pure (hd <> group pyLine <> "(" <> group pyLine <> tl <> group pyLine <> ")")

    pyLine = flatAlt (" \\"<> hardline) mempty

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
