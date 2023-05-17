(ns jamiep.euler549-v10i
  (:require [hyperfiddle.rcf :refer [tests]]))

(set! *warn-on-reflection* true)

;; version of v10 using mutation in place

(defn array-of-largest-prime-factors-for-all-int-to [max-n]
  (let [pfs (int-array (range 0 (inc max-n)))] 
    (loop [n 2]
      (if (<= n max-n)
        (do
          (when (= n (get pfs n))
            (loop [multiple (* n 2)]
              (when (<= multiple  max-n)
                (aset-int pfs multiple n)
                (recur (+ multiple n))))); found new prime
          (recur (inc n)))
        pfs))))

(tests (vec (array-of-largest-prime-factors-for-all-int-to 10)) := 
       [0 1 2 3 2 5 3 7 2 3 5])

(defn next-integer-with-factor [factor x]
  (+ x (- factor (mod x factor))))

(tests (next-integer-with-factor 2 10) := 12
       (next-integer-with-factor 7 10) := 14)

(defn product-of-integers-in-2-to-m-with-factor-p [m p]
  (apply *' (range p (inc m) p)))

(defn does-m!-contain-all-the-powers-of-p-in-x?
  [m x p]
  (loop [rem-m! (product-of-integers-in-2-to-m-with-factor-p m p)
         rem-x x]
    (if (zero? (mod rem-x p))
      (if (zero? (mod rem-m! p))
        (recur (quot rem-m! p) (quot rem-x p))
        false)
      true)))

(tests (does-m!-contain-all-the-powers-of-p-in-x? 5 10 2) := true)

(defn smallest-m-where-x-divides-m!
  "Used in a loop from 2 upwards.
   Given smallest previous m `prev-m` so that y divides `prev-m`
   (which we already calculated).
   And that x = y * p where p is prime.
   m is either `prev-m` or it is the next integer larger than `prev-m` 
   with factor p."
  [prev-m p x]
  (if (does-m!-contain-all-the-powers-of-p-in-x? prev-m x p)
    prev-m
    (next-integer-with-factor p prev-m)))

(defn array-of-smallest-m-till [^long q]
  (let [^ints pfs (array-of-largest-prime-factors-for-all-int-to q)
        s (int-array (inc q))] ; initialised to zero by default
    (loop [x 2]
      (if (<= x q)
        (let [p (aget pfs x)]
          (if (= p x)
            (aset-int s x x) ; prime so smallest-m = p = x
            (let [prev-m (aget s (quot x p))]
              (if (< prev-m p)
                (aset-int s x p)
                (aset-int s x (smallest-m-where-x-divides-m! prev-m p x)))))
          (recur (inc x)))
        s))))

(defn sum-of-smallest-m-till [^long q]
  (let [^ints s (array-of-smallest-m-till q)]
    (loop [x 2
           sum 0]
      (if (<= x q)
        (recur (inc x) (+ (aget s x) sum))
        sum))))

(tests (sum-of-smallest-m-till 100) := 2012)
