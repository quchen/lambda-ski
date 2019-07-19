{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}



module ExamplePrograms where

import Nominal     as N
import QuasiQuoter



factorial :: N.Expr
factorial = stdlib [nominal|
     Y (λrec n. (==0 n)
                1
                (* n (rec (pred n))))
    |]

fibonacci :: N.Expr
fibonacci = stdlib [nominal|
    Y (λrec n. (<= n 1)
               n
               (+ (rec (- n 1))
                  (rec (- n 2))))
    |]

helloWorld :: N.Expr
helloWorld = [nominal|
    (λ 1 2 + ^.
            (λ4 8 16 32 64 print.
                (λ H e l o  , ␣ w r d ! newline.
                     H (e (2 l (o (, (␣ (w (o (r (l (d (! (newline extern_eof))))))))))))
                )
                (print (+ 64 8))
                (print (+ 1 (+ 4 (+ 32 64))))
                (print (+ 4 (+ 8 (+ 32 64))))
                (print (+ 1 (+ 2 (+ 4 (+ 8 (+ 32 64))))))
                (print (+ 4 (+ 8 32)))
                (print 32)
                (print (+ 1 (+ 2 (+ 4 (+ 16 (+ 32 64))))))
                (print (+ 2 (+ 16 (+ 32 64))))
                (print (+ 4 (+ 32 64)))
                (print (+ 1 32))
                (print (+ 2 8))
            )
            (^ 2 2)
            (^ 2 (+ 1 2))
            (^ 2 (+ 2 2))
            (^ 2 (+ 1 (+ 2 2)))
            (^ 2 (+ 2 (+ 2 2)))
            (λn. extern_outChr (n extern_succ extern_0))
    )
    (λx. x)
    (λf x. f (f x))
    (λa b f x. a f (b f x))
    (λb e. e b)
    |]

stdlib :: N.Expr -> N.Expr
stdlib prog = define (Var "PROGRAM", prog) [nominal|
    (λ let.

    (let (λ x. x)           (λ id.
    (let (λ x _. x)         (λ const.
    (let (λ f g x. f (g x)) (λ compose.
    (let (λ f x y. f y x)   (λ flip.

    (let (λ {}. {}) (λ {}.

    (let (λ t _. t)          (λ True.
    (let (λ _ f. f)          (λ False.
    (let (λ p. p False True) (λ not.
    (let (λ p q. p q False)  (λ and.
    (let (λ p q. p True q)   (λ or.

    (let (λn f x. f (n f x))                         (λ succ.
    (let (λa b. a succ b)                            (λ +.
    (let (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) (λ pred.
    (let (λa b. b pred a)                            (λ -.
    (let (λm n f x. m (n f) x)                       (λ *.
    (let (λm n. n m)                                 (λ ^.

    (let (λ _ x. x)                  (λ 0.
    (let (succ 0)                    (λ 1.
    (let (succ 1)                    (λ 2.
    (let (succ 2)                    (λ 3.
    (let (succ 3)                    (λ 4.
    (let (succ 4)                    (λ 5.
    (let (succ 5)                    (λ 6.
    (let (succ 6)                    (λ 7.
    (let (succ 7)                    (λ 8.
    (let (succ 8)                    (λ 9.
    (let (λn rest. (* rest 10) + n)) (λ d.

    (let (λn. n (λ_. False) True)                       (λ ==0.
    (let (λ a b. and (==0 (- a b)) (==0 (- b a)))       (λ ==.
    (let (λ a b. and (==0 (- a b)) (not (==0 (- b a)))) (λ <.
    (let (flip <)                                       (λ >.
    (let (λ a b. not (== a b))                          (λ !=.
    (let (λ a b. not (< a b))                           (λ >=.
    (let (flip >=)                                      (λ <=.

    (let (λ f. (λ x. x x) (λ x. f (x x))) (λ Y.

    (let (λ n. n not True)  (λ even.
    (let (λ n. n not False) (λ odd.

    (let (λ a b p. p a b) (λ pair.
    (let (λ p. p True)    (λ fst.
    (let (λ p. p False)   (λ snd.

    (let (pair False)                   (λ Left.
    (let (pair True)                    (λ Right.
    (let (λ l r e. (fst e) r l (snd e)) (λ either.
    (let (compose not fst)              (λ isLeft.
    (let fst                            (λ isRight.
    (let (either ERROR id)              (λ fromRight.
    (let (either id ERROR)              (λ fromLeft.

    (let Right                      (λ Just.
    (let (Left {})                  (λ Nothing.
    (let (compose either const)     (λ maybe.
    (let (maybe id ERROR)           (λ fromJust.
    (let (maybe (const True) False) (λ isJust.
    (let (maybe (const False) True) (λ isNothing.

    (let (λ n _c. n)                                                                     (λ [].
    (let (λ x list. λ _n c. c x list)                                                    (λ :.
    (let (λ list. list True (λ _ _. False))                                              (λ null.
    (let (λ list. list ERROR (λ x _xs. x))                                               (λ head.
    (let (λ list. list ERROR (λ _x xs. xs))                                              (λ tail.
    (let (λ f z. (Y (λ rec list. null list z (f (head list) (rec (tail list))))))        (λ foldr.
    (let (λ f z list. foldr (λ x xs acc. xs (f acc x)) id list z)                        (λ foldl.
    (let (λ f. foldr (compose : f) [])                                                   (λ map.
    (let (λ f. foldr (λ x xs. (let (f x) (λ fx. isJust fx (: (fromJust fx) xs) xs) []))) (λ mapMaybe.
    (let (λ xs ys. foldr : ys xs)                                                        (λ ++.
    (let (λ f. (Y (λ rec xs ys. or (null xs) (null ys) [] (: (f (head xs) (head ys)) (rec (tail xs) (tail ys)))))) (λ zipWith.
    (let (zipWith pair)                                                                  (λ zip.
    (let (λf. Y (λ rec x. : x (rec (f x))))                                              (λ iterate.
    (let (compose Y :)                                                                   (λ repeat.
    (let (λ f. Y (λ rec x. let (f x) (λ fx. isJust fx (: (fst x) (rec (snd x))) [])))    (λ unfoldr.
    (let (λ n list. foldr (λ x xs k. ==0 k x (xs (pred k))) ERROR list n)                (λ index.
    (let (λ n list. foldr (λ x xs k. ==0 k [] (: x (xs (pred k)))) (const []) list n)    (λ take.
    (let (Y (λ rec n list. or (==0 n) (null list) list (rec (pred n) (tail list))))      (λ drop.
    (let (Y (λ rec n list.
        or (==0 n) (null list)
            (pair [] list)
            (let (rec (pred n) (tail list)) (λ rest.
                pair (: (head list) (fst rest)) (snd rest)))))                           (λ splitAt.
    (let (λ p. foldr (λ x xs. p x (: x) id xs) [])                                       (λ filter.
    (let (λ p. foldr (λ x xs. p x (: x xs) []) Nil)                                      (λ takeWhile.
    (let (λ p. Y (λ rec xs.
        null xs
            (pair [] [])
            (let (head xs) (λ x.
                (let (rec (tail xs)) (λ rest.
                    p x (pair (: x (fst rest)) (snd rest))
                        (pair (fst rest) (: x (snd rest)))))))))                         (λ partition.

    (let (λm. Y (λ rec n p.
             ==0 p
                 (+ m n)
             (and (==0 n) (== 1 p)
                 0
             (and (==0 n) (== 2 p)
                 1
             (==0 n
                 m
                 (rec m (rec m (pred n) p) (pred p)))))))
        (λ ackermann-phi.

    PROGRAM

    )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ) (λ value body. body value)
    |]
