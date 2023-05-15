(ns jamiep.euler549-v10i
  (:require [hyperfiddle.rcf :refer [tests]]))

;; version of v10 using mutation in place

(defn array-of-largest-prime-factors-for-all-int-to [^long max-n]
  (let [pfs (int-array (range 0 (inc max-n)))] 
    (loop [n 2]
      (if (<= n max-n)
        (do
          (when (= n (get pfs n))
            (loop [multiple (* n 2)]
              (when (<= multiple  max-n)
                (aset pfs multiple n)
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

(defn array-of-smallest-m-till' [^long q]
  (let [pfs (array-of-largest-prime-factors-for-all-int-to q)
        s (int-array (inc q))] ; initialised to zero by default
    (loop [x 2]
      (if (<= x q)
        (let [p (aget pfs x)]
          (if (= p x)
            (aset s x x) ; prime so smallest-m = p = x
            (let [prev-m (aget s (quot x p))]
              (if (< prev-m p)
                (aset s x p)
                (aset s x (smallest-m-where-x-divides-m! prev-m p x)))))
          (recur (inc x)))
        s))))

(defn array-of-smallest-m-till [q]
  (let [pfs (array-of-largest-prime-factors-for-all-int-to q)]
    (persistent!
     (reduce
      (fn [s x]
        (let [p (aget pfs x)]
          (if (= p x)
            (conj! s p) ; prime so m = p
            (let [prev-m (get s (quot x p))]
              (if (< prev-m p)
                (conj! s p)
                (conj! s (smallest-m-where-x-divides-m! prev-m p x)))))))
      (transient [0 0])
      (range 2 (inc q))))))

(tests
 (vec (array-of-smallest-m-till 10)) := [0 0 2 3 4 5 3 7 4 6 5])

(defn sum-of-smallest-m-till' [^long q]
  (let [s (array-of-smallest-m-till q)]
    (loop [x 2
           sum 0]
      (if (<= x q)
        (recur (inc x) (+ (aget s x) sum))
        sum))))

(defn sum-of-smallest-m-till [q]
  (let [s (array-of-smallest-m-till q)]
    (apply + s)))

(tests (sum-of-smallest-m-till 100) := 2012)

;; ;; (defn -main [& _args]
;; ;;   (doall (map #(let [n-to %
;; ;;                      p (Math/pow 10 %)]
;; ;;                  (println (str "sum of series to 1e" n-to))
;; ;;                  (println (time (sum-of-smallest-m-till p))))
;; ;;               (range 2 9))))


;; #_(time (sum-of-smallest-m-till (int (Math/pow 10 6))))