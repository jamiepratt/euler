(ns jamiep.euler549-v5
  (:require [hyperfiddle.rcf :refer [tests]]
            [jamiep.euler549-v4 :as v4]))

(defn v-p
  "p-adic valuation or p-adic order of an integer n
   is the exponent of the highest power of the prime number p
   that divides n"
  [n p]
  (loop [n n
         k 0]
    (if (zero? (mod n p))
      (recur (quot n p) (inc k))
      k)))

(tests
 (v-p 10 2) := 1
 (v-p 25 5) := 2
 (v-p 16 2) := 4
 (v-p 25 3) := 0
 (v-p 25 2) := 0)

(defn least-m
  "the least m such that p^k divides m!"
  [p k]
  (loop [r 0
         k k]
    (if (pos? k)
      (recur (inc r) (- k (inc (v-p (inc r) p))))
      (* p r))))

(tests
 (least-m 2 1) := 2
 (least-m 2 2) := 4
 (least-m 3 2) := 6
 (least-m 2 4) := 6)


(defn- sieve-update [l n]
  ;iterate through powers of prime n
  (loop [new-l l
         k 1
         u n]
    ;iterate through multiples of n
    (if (< u (count l))
      (let
       [i (range u (count l) u)
        s (least-m n k)
        to-replace (interleave
                    i
                    (for [j i]
                      (max (l j) s)))
        replaced (apply assoc
                        new-l
                        to-replace)]
        (tap> {:n n
               :u u
               :k k
               :l new-l
               :new-l replaced
               :to-replace to-replace})
        (recur replaced (inc k) (* u n)))
      new-l)))

(defn sieve-ff
  "sieve of Eratosthenes finding m! where m is the least number
   such that n divides m!
   ie. all powers of prime factors of n are in series 2..m"
  [max-n]
  (subvec
   (loop [l (vec (repeat (inc max-n) 0))
          n 2]
     (if (<= n max-n)
       (if (zero? (l n))
              ; p is prime
         (let [new-l (sieve-update l n)]
           (recur new-l (inc n)))
         (recur l (inc n)))
       l))
   2))


(tests
 (sieve-ff 7) := [2 3 4 5 3 7]
 (sieve-ff 16) := [2 3 4 5 3 7 4 6 5 11 4 13 7 5 6]
 (sieve-ff 30) := (v4/seq-of-smallest-m-till-p 30)
 (sieve-ff 100) := (v4/seq-of-smallest-m-till-p 100)
 )

(defn -main [& args]
  (doall (map #(let [p (int (Math/pow 10 %))]
                 (println "sum of prime factor series to " p)
                 (println (time (apply + (v4/seq-of-smallest-m-till-p p))))
                 (println (time (apply + (sieve-ff p)))))
              (range 2 8))))



