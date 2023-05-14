(ns jamiep.euler549-v10
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn seq-of-single-prime-factors-for-all-int-to [max-n]
  (loop [pfs (transient (into [] (range 0 (inc max-n))))
         n 2]
    (if (<= (Math/pow n 2) max-n)
      (let [new-pfs
            (if (= n (get pfs n))
              (reduce #(assoc! %1 %2 n)
                      pfs
                      (range (* n 2) (inc max-n) n)); found new prime
              pfs)]
        (recur new-pfs (inc n)))
      (persistent! pfs))))

(tests (seq-of-single-prime-factors-for-all-int-to 10)
       := [0 1 2 3 2 5 3 7 2 3 2])

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

(defn seq-of-smallest-m-till [q]
  (let [pfs (seq-of-single-prime-factors-for-all-int-to q)]
    (persistent!
     (reduce
      (fn [s x]
        (let [p (get pfs x)]
          (if (= p x)
            (conj! s p) ; prime so m = p
            (let [prev-m (get s (quot x p))]
              (conj! s (smallest-m-where-x-divides-m! prev-m p x))))))
      (transient [0 0])
      (range 2 (inc q))))))

(tests
 (seq-of-smallest-m-till 10) := [0 0 2 3 4 5 3 7 4 6 5])

(defn sum-of-smallest-m-till [q]
  (apply + (seq-of-smallest-m-till q)))

(tests (sum-of-smallest-m-till 100) := 2012)

;; (defn -main [& _args]
;;   (doall (map #(let [n-to %
;;                      p (Math/pow 10 %)]
;;                  (println (str "sum of series to 1e" n-to))
;;                  (println (time (sum-of-smallest-m-till p))))
;;               (range 2 9))))


#_(time (sum-of-smallest-m-till (int (Math/pow 10 6))))