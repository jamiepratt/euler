(ns jamiep.euler549-v7
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn factorial
  "n!"
  [n]
  (apply *' (range 2 (inc n))))

(defn v-p
  "p-adic valuation (also known as the p-adic order) of 
   n with respect to a prime number p.
   How many powers of p are factors of n?"
  [n p]
  (loop [n n
         k 0]
    (if (zero? (mod n p))
      (recur (quot n p) (inc k))
      k)))

;; v-p tests
(tests
 (v-p 10 2) := 1
 (v-p 25 5) := 2
 (v-p 16 2) := 4
 (v-p 25 3) := 0
 (v-p 25 2) := 0)

(defn v-p-of-m!
  "How many power of p are contained in m! ?"
  [m p]
  ;ignore parts of series 1..m which are not a factor of p.
  (apply + (map #(v-p % p) (range p (inc m) p))))

(tests
 (v-p-of-m! 6 2) :=3
 (map v-p-of-m! [11 10 8 6 4 2 1] (repeat 2)) 
 := (map v-p (map factorial [11 10 8 6 4 2 1]) (repeat 2)))

(defn- next-multiple-of-p [m p]
  (+ m (- p (mod m p))))

(tests (next-multiple-of-p 4 2) := 6
       (next-multiple-of-p 5 2) := 6
       (next-multiple-of-p 5 3) := 6
       (next-multiple-of-p 4 3) := 6)

(defn- increase-m
  "Find next least m such that u divides m!, 
   knowing that `u` is `p`^ `power` * q where q is some product of lower primes
   and that `last-m`! is smallest m divisible by q we can
   know that either `u` is already divisible by `last-m`! or the next multiple of `p`."
  [last-m p power]
  (loop [m last-m]
    (if (< (v-p-of-m! m p) power) ; does series 1..m contain all powers of p?
      (recur (next-multiple-of-p m p))
      m)))


(tests
 (increase-m 2 2 1) := 2
 (increase-m 2 3 1) := 3
 (increase-m 2 2 2) := 4
 (increase-m 2 2 3) := 4
 (increase-m 2 2 4) := 6)

(defn- next-p [m-series p n]
  (loop [p' p]
    (cond
      (>= p' n) nil
      (zero? (aget m-series p')) p'
      :else (recur (inc p')))))

(defn- next-prime-series
  "Given a prime `p` iterate through all the combinations of powers of that prime,
   then the products of combinations of all lower primes contained in `multipliers`."
  [m-series n multipliers p]
  (loop [multipliers' multipliers
         u-series [] ;series of products of combinations
         u p
         last-m 0
         power 1]
    (aset m-series u (increase-m last-m p power))
    (let [next-u (* u p)]
      (cond
        (< next-u n)
        ;continue through powers of p 
        (recur multipliers' (conj u-series u) next-u last-m (inc power))
        (seq multipliers')
        (recur (rest multipliers')
               (conj u-series u)
               (* p (first multipliers'))
               (aget m-series (first multipliers'))
               1)
        :else
        (concat multipliers (conj u-series u))))))                

(tests
 (def n 26)
 (def m-series (int-array n 0))
 (next-prime-series m-series n [] 2) := [2 4 8 16]
 (vec m-series) := [0, 0, 2, 0, 4, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0]

 (next-p m-series 2 n) := 3

 (filter #(< (* % *1) n) *3) = [2 4 8]

 (next-prime-series m-series n [2 4 8] 3) := [2 4 8 3 9 6 18 12 24]
 (vec m-series) := [0 0 2 3 4 0 3 0 4 6 0 0 4 0 0 0 6 0 6 0 0 0 0 0 4 0]

 (next-p m-series 3 n) := 5

 (filter #(< (* % *1) n) *3) = '(2 4 3)

 (next-prime-series m-series n *1 5) := [2 4 3 5 25 10 20 15]
 (vec m-series) := [0 0 2 3 4 5 3 0 4 6 5 0 4 0 0 5 6 0 6 0 5 0 0 0 4 10]

 (next-p m-series 5 n) := 7
 (filter #(< (* % *1) n) *3) = '(2 3)
 (next-prime-series m-series n *1 7) := [2 3 7 14 21]
 (vec m-series) := [0 0 2 3 4 5 3 7 4 6 5 0 4 0 7 5 6 0 6 0 5 7 0 0 4 10]
 
 (next-p m-series 7 n) := 11
 (filter #(< (* % *1) n) *3) = [2]
 (next-prime-series m-series n *1 11) := [2 11 22]
 (vec m-series) := [0 0 2 3 4 5 3 7 4 6 5 11 4 0 7 5 6 0 6 0 5 7 11 0 4 10]

 (next-p m-series 11 n) := 13
 (filter #(< (* % *1) n) *3) = []
 (next-prime-series m-series n *1 13) := [13]
 (vec m-series) := [0 0 2 3 4 5 3 7 4 6 5 11 4 13 7 5 6 0 6 0 5 7 11 0 4 10]

 (next-p m-series 13 n) := 17
(filter #(< (* % *1) n) *3) = []
(next-prime-series m-series n *1 17) := [17]
(vec m-series) := [0 0 2 3 4 5 3 7 4 6 5 11 4 13 7 5 6 17 6 0 5 7 11 0 4 10]
 
 (next-p m-series 17 n) := 19
(filter #(< (* % *1) n) *3) = []
(next-prime-series m-series n *1 19) := [19]
(vec m-series) := [0 0 2 3 4 5 3 7 4 6 5 11 4 13 7 5 6 17 6 19 5 7 11 0 4 10]

(next-p m-series 19 n) := 23
(filter #(< (* % *1) n) *3) = []
(next-prime-series m-series n *1 23) := [23]
(vec m-series) := [0 0 2 3 4 5 3 7 4 6 5 11 4 13 7 5 6 17 6 19 5 7 11 23 4 10])

(defn sieve-ff
  "Find the smallest m for all integers up to to-n."
  [to-n]
  (let [n (inc to-n)
        ;m-series will be the list of smallest m for all integers up to to-n
        m-series (int-array n 0)]
    (loop [p 2
           last-products-of-powers []]
      (if-let [p' (next-p m-series p n)]
        (let [last-products-of-powers' (filter #(< (* % p') n) last-products-of-powers)
              next-products-series (next-prime-series
                                    m-series
                                    n
                                    last-products-of-powers'
                                    p')]
          (recur p' next-products-series))
        (reduce #(+ %1 (aget m-series %2)) 0 (range 2 n))))))

(tests
 (sieve-ff 2) := 2
 (sieve-ff 3) := 5
 (sieve-ff 25) := 187
 (sieve-ff 100) := 2012)

