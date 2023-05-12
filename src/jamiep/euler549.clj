(ns jamiep.euler549
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn factorial [n]
  (reduce *' (range 1 (inc n))))

(defn smallest-m-series-sieve' [n]
  (loop [nums {}
         i 2]
    (cond
      (> i n) nums
      :else (recur (conj nums [i (range (* i 2)
                                        (min n (inc (factorial i)))
                                        i)])
                            (inc i))
      )))

(comment (smallest-m-series-sieve' 100))



(defn smallest-m-series-sieve [n]
  (loop [nums (transient (vec (range n)))
         i 2]
    (cond
      (> i n) (nnext (persistent! nums))
      (= i (nums i)) (recur (reduce #(assoc! %1 %2 i) nums
                              (range (* i 2)
                                     (min n (inc (factorial i)))
                                     i))
                      (inc i))
      :else (recur nums (inc i)))))

(defn smallest-m-naive
  "Brute force search for the smallest number m such that n divides m!"
  [n]
  (first (filter #(zero? (rem (factorial %) n)) (range (inc n)))))

(defn smallest-m-series-naive [n]
  (map smallest-m-naive (range 2 n)))

(tests
 (smallest-m-series-sieve 10) := (smallest-m-series-naive 10)

 (smallest-m-series-sieve 20) := (smallest-m-series-naive 20)
 )

(comment (smallest-m-naive 10))
;; The smallest number m such that 10 divides m! is m=5.

; because 5! = 5 * 4 * 3 * 2 * 1
; and 10 = 5 * 2 so 10 is divisible by 5! because 5! has a 5 and a 2 in it


;; The smallest number m such that 25 divides m! is m=10.

;; Let s (n) be the smallest number m such that n divides m!.

;; So s (10) = 5 and s (25) = s(5^2) = 10.

;s(2) = 2
;s(3) = 3
;s(4) = 4
;s(5) = 5
;s(6) = s(2*3) = 3 (because 3! = 1*2*3)
;s(7) = 7
;s(8) = s(2 ^ 3) = 4 (because 4! = 1*2*3*4 which has 3 2s in it)
;s(9) = 6 (because 6! = 1*2*3*4*5*6 which has 2 3s in it)

;; Let S (n) be ∑s (i) for 2 ≤ i ≤ n.
;; S (100) =2012.

;; Find S (10^8) .
