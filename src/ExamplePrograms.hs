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

stdlib :: N.Expr
stdlib =
    " (λ let.                                                                                                      \
    \ (let (λ x. x)   (λ id.                                                                                       \
    \ (let (λ x _. x) (λ const.                                                                                    \
    \                                                                                                              \
    \ (let (λ t _. t)          (λ true.                                                                            \
    \ (let (λ _ f. f)          (λ false.                                                                           \
    \ (let (λ p. p false true) (λ not.                                                                             \
    \ (let (λ p q. p q false)  (λ and.                                                                             \
    \ (let (λ p q. p true q)   (λ or.                                                                              \
    \                                                                                                              \
    \ (let (λ f x. x)                                  (λ 0.                                                       \
    \ (let (λ f x. f x)                                (λ 1.                                                       \
    \ (let (λ f x. f (f x))                            (λ 2.                                                       \
    \ (let (λ f x. f (f (f x)))                        (λ 3.                                                       \
    \ (let (λn f x. f (n f x))                         (λ succ.                                                    \
    \ (let (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) (λ pred.                                                    \
    \ (let (λa b f x. a f (b f x))                     (λ +.                                                       \
    \ (let (λa b f x. b pred a)                        (λ -.                                                       \
    \ (let (λm n f x. m (n f) x)                       (λ *.                                                       \
    \ (let (λm n. n m)                                 (λ ^.                                                       \
    \                                                                                                              \
    \ (let (λn. n (λ_. false) true)                       (λ ==0.                                                  \
    \ (let (λ a b. and (==0 (- a b)) (==0 (- b a)))       (λ ==.                                                   \
    \ (let (λ a b. and (==0 (- a b)) (not (==0 (- b a)))) (λ <.                                                    \
    \ (let (λ a b. < b a)                                 (λ >.                                                    \
    \ (let (λ a b. not (== a b))                          (λ !=.                                                   \
    \ (let (λ a b. not (< a b))                           (λ >=.                                                   \
    \ (let (λ a b. >= b a)                                (λ <=.                                                   \
    \                                                                                                              \
    \ (let (λ f. (λ x. f (x x)) (λ x. f (x x))) (λ Y.                                                              \
    \                                                                                                              \
    \ (let (λ a b p. p a b) (λ pair.                                                                               \
    \ (let (λ p. p true)    (λ fst.                                                                                \
    \ (let (λ p. p false)   (λ snd.                                                                                \
    \                                                                                                              \
    \ (let false                                                                                       (λ nil.     \
    \ (let (λ list. list (λ _ _. false) true)                                                          (λ null.    \
    \ (let pair                                                                                        (λ cons.    \
    \ (let fst                                                                                         (λ head.    \
    \ (let snd                                                                                         (λ tail.    \
    \ (let (λ f z. (Y rec list. null list z (f (head list) (rec (tail list)))))                        (λ foldr.   \
    \ (let (λf. (Y (λ rec x. cons x (rec (f x)))))                                                     (λ iterate. \
    \ (let (λx. (Y (λ rec. cons x rec)))                                                               (λ repeat.  \
    \ (let (Y (λ rec n list. ==0 n nil  (null list nil (cons (head list) (rec (- n 1) (tail list)))))) (λ take.    \
    \ (let (Y (λ rec n list. ==0 n list (null list nil (                 (rec (- n 1) (tail list)))))) (λ drop.    \
    \ (let (λ p. Y (λ rec list. p (head list) (cons (head list)) id (rec (tail list))))                (λ filter.  \
    \ (let (λ p. Y (λ rec list. p (head list) (cons (head list) (rec (tail list))) nil))               (λ takeWhile. \
    \                                                                                                              \
    \ (let (λm. Y (λ rec n p.                                                                                      \
    \          ==0 p                                                                                               \
    \              (+ m n)                                                                                         \
    \          (and (==0 n) (== 1 p)                                                                               \
    \              0                                                                                               \
    \          (and (==0 n) (== 2 p)                                                                               \
    \              1                                                                                               \
    \          (==0 n                                                                                              \
    \              m                                                                                               \
    \              (rec m (rec m (- n 1) p) (- p 1)))))))                                                          \
    \     (λ ackermann-phi.                                                                                        \
    \ (BODY)                                                                                                       \
    \ ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))                           \
    \ ) (λ definition body. body definition)                                                                       "
