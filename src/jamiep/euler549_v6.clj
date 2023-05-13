(ns jamiep.euler549-v6
  (:require [hyperfiddle.rcf :refer [tests]]
            [jamiep.euler549-v4 :as v4]))


(defn a-prime-factor-to [^long n]
  (let [prime-factors (int-array (inc n) (range (inc n)))
        sqrt-n (int (Math/ceil (Math/sqrt n)))]
    (loop [p 2]
      (if (< sqrt-n p)
        prime-factors
        (do
          (when (= p (aget prime-factors p))
            ;found new prime
            ;update multiples of p
            (loop [i (* p 2)]
              (when (<= i n)
                (aset prime-factors i p)
                (recur (+ i p)))))
          (recur (inc p)))))))


(defn factors [i primes]
  (loop [remainder i
         factors {}]
    (if (= remainder 1)
      factors
      (let [prime (aget primes remainder)]
        (recur (quot remainder prime) (update factors prime (fnil inc 0)))))))

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


(defn least-m-for-one-prime
  "the least m such that p^k divides m!"
  [p k]
  (loop [r 0
         k k]
    (if (pos? k)
      (recur (inc r) (- k (inc (v-p (inc r) p))))
      (* p r))))

(defn least-m [factors]
  (reduce (fn [acc [p k]] (max acc (least-m-for-one-prime p k)))
          1
          factors))

(tests
 (least-m {2 1}) := 2
 (least-m {3 1}) := 3
 (least-m {2 2}) := 4
 (least-m {5 1}) := 5
 (least-m {3 1, 2 1}) := 3
 (least-m {7 1}) := 7
 (least-m {2 3}) := 4
 (least-m {3 2}) := 6
 (least-m {5 1, 2 1}) := 5
 (least-m {11 1}) := 11
 (least-m {3 1, 2 2}) := 4
 (least-m {13 1}) := 13
 (least-m {2 1, 7 1}) := 7
 (least-m {5 1, 3 1}) := 5
 (least-m {2 4}) := 6
 (least-m {17 1}) := 17
 (least-m {3 2, 2 1}) := 6
 (least-m {19 1}) := 19
 (least-m {5 1, 2 2}) := 5)

(tests
 (map #(least-m {2 %}) (range 1 10)) := '(2 4 4 6 8 8 8 10 12)
 (map #(least-m {3 %}) (range 1 10)) := '(3 6 9 9 12 15 18 18 21)
 (map #(least-m {5 %}) (range 1 10)) := '(5 10 15 20 25 25 30 35 40)
 (map #(least-m {7 %}) (range 1 10)) := '(7 14 21 28 35 42 49 49 56)
 (map #(least-m {11 %}) (range 1 10)) := '(11 22 33 44 55 66 77 88 99))


(defn least-m-to [^long n]
  (let [primes (a-prime-factor-to n)]
    (loop [i 2
           series (int-array (dec n) 1)]
      (aset series (- i 2) (least-m (factors i primes)))
      (if (< i n)
        (recur (inc i) series)
        series))))

(tests
 (apply + (least-m-to 100)) := 2012
 (vec (least-m-to 20)) := [2, 3, 4, 5, 3, 7, 4, 6, 5, 11, 4, 13, 7, 5, 6, 17, 6, 19, 5]
 (count (least-m-to 20)) := 19
 (drop 19 (vec (least-m-to 40))) := [7 11 23 4 10 13 9 7 29 5 31 8 11 17 7 6 37 19 13 5]
 (drop 39 (vec (least-m-to 60))) := [41 7 43 11 6 23 47 6 14 10 17 13 53 9 11 7 19 29 59 5]
 (drop 59 (vec (least-m-to 80))) := '(61 31 7 8 13 11 67 17 23 7 71 6 73 37 10 19 11 13 79 6)
 (vec (least-m-to 200)) := (v4/seq-of-smallest-m-till 200)
 )
nil
