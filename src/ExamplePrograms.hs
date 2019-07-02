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
    " (λ hask_outChr hask_eof hask_succ hask_0.                                                   \
    \     (λ 1 + *.                                                                               \
    \         (λ 2 ^.                                                                              \
    \             (λ 4 8 16 32 64 print ;.                                                           \
    \                 (λ H e l o , ␣ w r d ! LF.                                              \
    \                      H e l l o , ␣ w o r l d ! LF ;             \
    \                 )                                                                           \
    \                 (print ((λ_. 2) (+ 64 8)))                                                            \
    \                 (print ((λ_. 2) (+ 1 (+ 4 (+ 32 64)))))                                               \
    \                 (print ((λ_. 2) (+ 4 (+ 8 (+ 32 64)))))                                               \
    \                 (print ((λ_. 2) (+ 1 (+ 2 (+ 4 (+ 8 (+ 32 64)))))))                                   \
    \                 (print ((λ_. 2) (+ 4 (+ 8 32))))                                                      \
    \                 (print ((λ_. 2) 32))                                                                  \
    \                 (print ((λ_. 2) (+ 1 (+ 2 (+ 4 (+ 16 (+ 32 64)))))))                                  \
    \                 (print ((λ_. 2) (+ 2 (+ 16 (+ 32 64)))))                                              \
    \                 (print ((λ_. 2) (+ 4 (+ 32 64))))                                                     \
    \                 (print ((λ_. 2) (+ 1 32)))                                                            \
    \                 (print ((λ_. 2) (+ 2 8)))                                                             \
    \             )                                                                               \
    \             (^ 2 2)                                                                         \
    \             (^ 2 (+ 1 2))                                                                   \
    \             (^ 2 (+ 2 2))                                                                   \
    \             (^ 2 (+ 1 (+ 2 2)))                                                             \
    \             (^ 2 (+ 2 (+ 2 2)))                                                             \
    \             (λn cont. cont (hask_outChr (n hask_succ hask_0)))                                          \
    \             hask_eof                                                                    \
    \         )                                                                                   \
    \         (+ 1 1)                                                                             \
    \         (λb e. e (* b) 1)                                                                   \
    \     )                                                                                       \
    \     (λf x. f x)                                                                             \
    \     (λa b. λf x. a f (b f x))                                                               \
    \     (λa b. λf x. a (b f) x)                                                                 \
    \ )                                                                                           "
