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
    λn. index n (Y (λ rec. : 0 (: 1 (zipWith + rec (tail rec)))))
    |]

helloWorld :: N.Expr
helloWorld = [nominal|
    (λ let.
        let (λa b f x. a f (b f x)) (λ +.

        let (λf x. f x) (λ 1.
        let (+ 1 1)     (λ 2.
        let (+ 2 2)     (λ 4.
        let (+ 4 4)     (λ 8.
        let (+ 8 8)     (λ 16.
        let (+ 16 16)   (λ 32.
        let (+ 32 32)   (λ 64.

        let (λn. extern_outChr (n extern_succ extern_0)) (λ print.
        let (print (+ 8 64))                             (λ H.
        let (print (+ 1 (+ 4 (+ 32 64))))                (λ e.
        let (print (+ 4 (+ 8 (+ 32 64))))                (λ l.
        let (print (+ 1 (+ 2 (+ 4 (+ 8 (+ 32 64))))))    (λ o.
        let (print (+ 4 (+ 8 32)))                       (λ ,.
        let (print 32)                                   (λ ␣.
        let (print (+ 1 (+ 2 (+ 4 (+ 16 (+ 32 64))))))   (λ w.
        let (print (+ 2 (+ 16 (+ 32 64))))               (λ r.
        let (print (+ 4 (+ 32 64)))                      (λ d.
        let (print (+ 1 32))                             (λ !.
        let (print (+ 2 8))                              (λ newline.

        H (e (2 l (o (, (␣ (w (o (r (l (d (! (newline extern_eof))))))))))))

    ))))))))))))))))))))
    ) (λ value body. body value)
    |]

stdlib :: N.Expr -> N.Expr
stdlib prog = define (Var "PROGRAM", prog) [nominal|
    (λ let.

    let (λ x. x)           (λ id.
    let (λ x _. x)         (λ const.
    let (λ f g x. f (g x)) (λ compose.
    let (λ f x y. f y x)   (λ flip.

    let (λ {}. {}) (λ {}.

    let (λ t _. t)     (λ True.
    let (λ _ f. f)     (λ False.
    let flip           (λ not.
    let (λ p q. p q p) (λ and.
    let (λ p q. p p q) (λ or.

    let (λn f x. f (n f x))                         (λ succ.
    let (λa b. a succ b)                            (λ +.
    let (λn f x. n (λg h. h (g f)) (λ_. x) (λu. u)) (λ pred.
    let (λa b. b pred a)                            (λ -.
    let (λm n f x. m (n f) x)                       (λ *.
    let (λm n. n m)                                 (λ ^.

    let (λ _ x. x)                 (λ 0.
    let (succ 0)                   (λ 1.
    let (succ 1)                   (λ 2.
    let (succ 2)                   (λ 3.
    let (succ 3)                   (λ 4.
    let (succ 4)                   (λ 5.
    let (succ 5)                   (λ 6.
    let (succ 6)                   (λ 7.
    let (succ 7)                   (λ 8.
    let (succ 8)                   (λ 9.
    let (λn rest. (* rest 10) + n) (λ d.

    let (λn. n (λ_. False) True)                       (λ ==0.
    let (λ a b. and (==0 (- a b)) (==0 (- b a)))       (λ ==.
    let (λ a b. and (==0 (- a b)) (not (==0 (- b a)))) (λ <.
    let (flip <)                                       (λ >.
    let (λ a b. not (== a b))                          (λ !=.
    let (λ a b. not (< a b))                           (λ >=.
    let (flip >=)                                      (λ <=.

    let (λ f. (λ x. x x) (λ x. f (x x))) (λ Y.

    let (λ n. n not True)  (λ even.
    let (λ n. n not False) (λ odd.

    let (λ a b p. p a b)                         (λ pair.
    let (λ f p. f (p (λ x _. x)) (p (λ _ y. y))) (λ uncurry.
    let (uncurry (λ f _. f))                     (λ fst.
    let (uncurry (λ _ s. s))                     (λ snd.

    let (λ x l _. l x)                      (λ Left.
    let (λ x _ r. r x)                      (λ Right.
    let (λ l r e. e l r)                    (λ either.
    let (either (const True) (const False)) (λ isLeft.
    let (compose not isLeft)                (λ isRight.
    let (either ERROR id)                   (λ fromRight.
    let (either id ERROR)                   (λ fromLeft.

    let (λ x _ j. j x)             (λ Just.
    let (λ n _. n)                 (λ Nothing.
    let (λ n j x. x n j)           (λ maybe.
    let (maybe ERROR id)           (λ fromJust.
    let (maybe False (const True)) (λ isJust.
    let (maybe True (const False)) (λ isNothing.

    let (λ n _c. n)                                                                  (λ [].
    let (λ x xs _n c. c x xs)                                                        (λ :.
    let (λ z f list. list z (λ x xs. f x xs))                                        (λ case[].
    let (case[] True (λ _ _. False))                                                 (λ null.
    let (case[] ERROR (λ x _xs. x))                                                  (λ head.
    let (case[] ERROR (λ _x xs. xs))                                                 (λ tail.
    let (λ f z. (Y (λ rec. case[] z (λ x xs. f x (rec xs)))))                        (λ foldr.
    let (λ f z list. foldr (λ x xs acc. xs (f acc x)) id list z)                     (λ foldl.
    let (λ f. foldr (compose : f) [])                                                (λ map.
    let (λ f. foldr (λ x xs. (maybe xs (λ fx. : fx xs) (f x))) [])                   (λ mapMaybe.
    let (λ xs ys. foldr : ys xs)                                                     (λ ++.
    let (λ f. (Y (λ rec xx yy. case[] [] (λ x xs. case[] [] (λ y ys. : (f x y) (rec xs ys)) yy) xx))) (λ zipWith.
    let (zipWith pair)                                                               (λ zip.
    let (foldr (pair [] []) (λ xy xys. uncurry (λ x y. uncurry (λ xs ys. pair (: x xs) (: y ys)) xys) xy)) (λ unzip.
    let (λf. Y (λ rec x. : x (rec (f x))))                                           (λ iterate.
    let (compose Y :)                                                                (λ repeat.
    let (λ f. Y (λ rec x. (maybe [] (λ x. uncurry (λ fs sn. : x fs (rec sn))))))     (λ unfoldr.
    let (λ n list. foldr (λ x xs k. ==0 k x (xs (pred k))) ERROR list n)             (λ index.
    let (λ n list. foldr (λ x xs k. ==0 k [] (: x (xs (pred k)))) (const []) list n) (λ take.
    let (Y (λ rec n list. or (==0 n) (null list) list (rec (pred n) (tail list))))   (λ drop.
    let (Y (λ rec n list.
        or (==0 n) (null list)
            (pair [] list)
            (let (rec (pred n) (tail list))
                (uncurry (λ fs sn. pair (: (head list) fs) sn)))))                   (λ splitAt.
    let (λ p. foldr (λ x xs. p x (: x) id xs) [])                                    (λ filter.
    let (λ p. foldr (λ x xs. p x (: x xs) []) [])                                    (λ takeWhile.
    let (λ p. Y (λ rec list. case[] [] (λ x xs. p x (rec xs) list) list))            (λ dropWhile.
    let (λ p. Y (λ rec.
        case[]
            (pair [] [])
            (λ x xs. uncurry (λ fs sn. p x (pair (: x fs) sn)
                                           (pair fs (: x sn)))
                             (rec xs))))                                             (λ partition.

    let (λm. Y (λ rec n p.
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

    ))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    ) (λ value body. body value)
    |]
