{-# LANGUAGE OverloadedStrings #-}

module ExamplePrograms where

import Nominal as N

factorial :: N.Expr
factorial =
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
fibonacci =
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
helloWorld =
    " (λ 1 2 + ^.                                                                           \
    \         (λ4 8 16 32 64 print.                                                         \
    \             (λ H e l o  , ␣ w r d ! newline.                                          \
    \                  H (e (2 l (o (, (␣ (w (o (r (l (d (! (newline extern_eof)))))))))))) \
    \             )                                                                         \
    \             (print (+ 64 8))                                                          \
    \             (print (+ 1 (+ 4 (+ 32 64))))                                             \
    \             (print (+ 4 (+ 8 (+ 32 64))))                                             \
    \             (print (+ 1 (+ 2 (+ 4 (+ 8 (+ 32 64))))))                                 \
    \             (print (+ 4 (+ 8 32)))                                                    \
    \             (print 32)                                                                \
    \             (print (+ 1 (+ 2 (+ 4 (+ 16 (+ 32 64))))))                                \
    \             (print (+ 2 (+ 16 (+ 32 64))))                                            \
    \             (print (+ 4 (+ 32 64)))                                                   \
    \             (print (+ 1 32))                                                          \
    \             (print (+ 2 8))                                                           \
    \         )                                                                             \
    \         (^ 2 2)                                                                       \
    \         (^ 2 (+ 1 2))                                                                 \
    \         (^ 2 (+ 2 2))                                                                 \
    \         (^ 2 (+ 1 (+ 2 2)))                                                           \
    \         (^ 2 (+ 2 (+ 2 2)))                                                           \
    \         (λn. extern_outChr (n extern_succ extern_0))                                  \
    \ )                                                                                     \
    \ (λx. x)                                                                               \
    \ (λf x. f (f x))                                                                       \
    \ (λa b f x. a f (b f x))                                                               \
    \ (λb e. e b)                                                                           "

stdlib :: N.Expr -> N.Expr
stdlib prog = define (Var "PROGRAM", prog)
    " (λ let.                   \n\
    \ (let (λ x. x)   (λ id.    \n\
    \ (let (λ x _. x) (λ const. \n\
    \\n\
    \ (let (λ t _. t)          (λ true.  \n\
    \ (let (λ _ f. f)          (λ false. \n\
    \ (let (λ p. p false true) (λ not.   \n\
    \ (let (λ p q. p q false)  (λ and.   \n\
    \ (let (λ p q. p true q)   (λ or.    \n\
    \\n\
    \ (let (λ f x. x)                                  (λ 0.    \n\
    \ (let (λ f x. f x)                                (λ 1.    \n\
    \ (let (λ f x. f (f x))                            (λ 2.    \n\
    \ (let (λ f x. f (f (f x)))                        (λ 3.    \n\
    \ (let (λa b f x. a f (b f x))                     (λ +.    \n\
    \ (let (+ 1)                                       (λ succ. \n\
    \ (let (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) (λ pred. \n\
    \ (let (λa b. b pred a)                            (λ -.    \n\
    \ (let (λm n f x. m (n f) x)                       (λ *.    \n\
    \ (let (λm n. n m)                                 (λ ^.    \n\
    \\n\
    \ (let (λn. n (λ_. false) true)                       (λ ==0. \n\
    \ (let (λ a b. and (==0 (- a b)) (==0 (- b a)))       (λ ==.  \n\
    \ (let (λ a b. and (==0 (- a b)) (not (==0 (- b a)))) (λ <.   \n\
    \ (let (λ a b. < b a)                                 (λ >.   \n\
    \ (let (λ a b. not (== a b))                          (λ !=.  \n\
    \ (let (λ a b. not (< a b))                           (λ >=.  \n\
    \ (let (λ a b. >= b a)                                (λ <=.  \n\
    \\n\
    \ (let (λ f. (λ x. x x) (λ x. f (x x))) (λ Y. \n\
    \\n\
    \ (let (λ a b p. p a b) (λ pair. \n\
    \ (let (λ p. p true)    (λ fst.  \n\
    \ (let (λ p. p false)   (λ snd.  \n\
    \\n\
    \ (let false                                                                            (λ nil.       \n\
    \ (let (λ list. list (λ _ _ _. false) true)                                             (λ null.      \n\
    \ (let pair                                                                             (λ cons.      \n\
    \ (let fst                                                                              (λ head.      \n\
    \ (let snd                                                                              (λ tail.      \n\
    \ (let (λ f z. (Y (λ rec list. null list z (f (head list) (rec (tail list))))))         (λ foldr.     \n\
    \ (let (λ f z list. foldr (λ x xs acc. xs (f acc x)) id list z)                         (λ foldl.     \n\
    \ (let (λ f. foldr (λ x xs. cons (f x) xs) nil)                                         (λ map.       \n\
    \ (let (λf. Y (λ rec x. cons x (rec (f x))))                                            (λ iterate.   \n\
    \ (let (λx. Y (cons x))                                                                 (λ repeat.    \n\
    \ (let (λ n list. foldr (λ x xs k. ==0 k x (xs (- k 1)))            ERROR       list n) (λ index.     \n\
    \ (let (λ n list. foldr (λ x xs k. ==0 k nil (cons x (xs (- k 1)))) (const nil) list n) (λ take.      \n\
    \ (let (λ n list. foldr (λ x xs k. ==0 k xs (xs (- k 1)))           (const nil) list n) (λ drop.      \n\
    \ (let (λ p. foldr (λ x xs. p x (cons x) id xs) nil)                                    (λ filter.    \n\
    \ (let (λ p. foldr (λ x xs. p x (cons x xs) nil) nil)                                   (λ takeWhile. \n\
    \\n\
    \ (let (λm. Y (λ rec n p.                             \n\
    \          ==0 p                                      \n\
    \              (+ m n)                                \n\
    \          (and (==0 n) (== 1 p)                      \n\
    \              0                                      \n\
    \          (and (==0 n) (== 2 p)                      \n\
    \              1                                      \n\
    \          (==0 n                                     \n\
    \              m                                      \n\
    \              (rec m (rec m (- n 1) p) (- p 1))))))) \n\
    \     (λ ackermann-phi.                               \n\
    \\n\
    \ PROGRAM \
    \\n\
    \ )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) \
    \ ) (λ value body. body value) "
