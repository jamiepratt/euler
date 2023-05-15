(ns jamiep.euler10-v1
  (:require [hyperfiddle.rcf :refer [tests]]))

;; https://projecteuler.net/problem=10
;; Summation of primes

;; Problem 10
;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

;; Find the sum of all the primes below two million.

(defn sum-of-all-primes-to [n]
  (let [to-n (inc n)
        prime? (boolean-array to-n true) ]
    (reduce
     (fn [total x]
       (if (aget prime? x)
         (do
           (doseq [y (range x to-n x)] (aset prime? y false))
           (+ total x))
         total))
     2
     (range 3 to-n 2))))

(tests (sum-of-all-primes-to 10) := 17)

(time (sum-of-all-primes-to 2000000))

;; there are sum of primes algorithms that are more efficient
;; https://projecteuler.net/thread=10;page=5