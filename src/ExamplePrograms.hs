{-# LANGUAGE OverloadedStrings #-}

module ExamplePrograms where

import Nominal as N

factorial :: N.Expr
factorial = N.unsafeParse
    " (λ-1 * true false Y 1.                      \
    \     (λ =0.                                  \
    \         Y (λrec n. (=0 n)                   \
    \                    1                        \
    \                    (* n (rec (-1 n)))       \
    \           )                                 \
    \     )                                       \
    \     (λn. n (λ_. false) true)                \
    \ )                                           \
    \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) \
    \ (λm n f x. m (n f) x)                       \
    \ (λt _. t)                                   \
    \ (λ_ f. f)                                   \
    \ (λf. (λx. f (x x)) (λx. f (x x)))           \
    \ (λf x. f x)                                 "

fibonacci :: N.Expr
fibonacci = N.unsafeParse
    " (λ +1 -1 true false Y 1.                    \
    \     (λ =0 + -.                              \
    \         (λ <= 2.                            \
    \             Y (λrec n. (<= n 1)             \
    \                        n                    \
    \                        (+ (rec (- n 1))     \
    \                           (rec (- n 2))))   \
    \         )                                   \
    \         (λm n. =0 (- m n))                  \
    \         (+ 1 1)                             \
    \     )                                       \
    \     (λn. n (λ_. false) true)                \
    \     (λm n. m +1 n)                          \
    \     (λm n. n -1 m)                          \
    \ )                                           \
    \ (λn f x. f (n f x))                         \
    \ (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) \
    \ (λt _. t)                                   \
    \ (λ_ f. f)                                   \
    \ (λf. (λx. f (x x)) (λx. f (x x)))           \
    \ (λf x. f x)                                 "

helloWorld :: N.Expr
helloWorld = N.unsafeParse
    " (λ hask_outChr hask_succ hask_0 hask_eof.                                                     \
    \     (λ 1 + *.                                                                                 \
    \         (λ2 ^.                                                                                \
    \             (λ4 8 16 32 64 print.                                                             \
    \                 (λ H e l o , space w r d ! newline.                                           \
    \                      H (e (l (l (o (, (space (w (o (r (l (d (! (newline hask_eof))))))))))))) \
    \                 )                                                                             \
    \                 (print (+ 64 8))                                                              \
    \                 (print (+ 1 (+ 4 (+ 32 64))))                                                 \
    \                 (print (+ 4 (+ 8 (+ 32 64))))                                                 \
    \                 (print (+ 1 (+ 2 (+ 4 (+ 8 (+ 32 64))))))                                     \
    \                 (print (+ 4 (+ 8 32)))                                                        \
    \                 (print 32)                                                                    \
    \                 (print (+ 1 (+ 2 (+ 4 (+ 16 (+ 32 64))))))                                    \
    \                 (print (+ 2 (+ 16 (+ 32 64))))                                                \
    \                 (print (+ 4 (+ 32 64)))                                                       \
    \                 (print (+ 1 32))                                                              \
    \                 (print (+ 2 8))                                                               \
    \             )                                                                                 \
    \             (^ 2 2)                                                                           \
    \             (^ 2 (+ 1 2))                                                                     \
    \             (^ 2 (+ 2 2))                                                                     \
    \             (^ 2 (+ 1 (+ 2 2)))                                                               \
    \             (^ 2 (+ 2 (+ 2 2)))                                                               \
    \             (λn. hask_outChr (n hask_succ hask_0))                                            \
    \         )                                                                                     \
    \         (+ 1 1)                                                                               \
    \         (λb e. e (* b) 1)                                                                     \
    \     )                                                                                         \
    \     (λf x. f x)                                                                               \
    \     (λa b. λf x. a f (b f x))                                                                 \
    \     (λa b. λf x. a (b f) x)                                                                   \
    \ )                                                                                             "
