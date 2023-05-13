(ns jamiep.euler549-v9
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn seq-of-single-prime-factors-for-all-int-to [max-n]
  (loop [pfs (transient (into [] (range 0 (inc max-n))))
         n 2]
    (if (<= (Math/pow n 2) max-n)
      (let [new-pfs (if (= n (get pfs n))
                      (reduce #(assoc! %1 %2 n)
                              pfs
                              (range (* n 2) (inc max-n) n))     ; found new prime
                      pfs)]
        (recur new-pfs (inc n)))
      (persistent! pfs))))

(defn prime-factors-of [pre-calculated-pf-series x]
  (loop [to-factorise x
         factors (transient #{})]
    (let [factor (get pre-calculated-pf-series to-factorise)]
      (if (not= to-factorise 1)
        (recur (quot to-factorise factor) (conj! factors factor))
        (persistent! factors)))))

(defn seq-of-sets-of-all-prime-factors-for-all-to [max-n]
  (let [pfs (seq-of-single-prime-factors-for-all-int-to max-n)]
    (mapv #(if (< % 3) #{%} (prime-factors-of pfs %))
          (range (inc max-n)))))

(tests (seq-of-sets-of-all-prime-factors-for-all-to 10) :=
       [#{0} #{1} #{2} #{3} #{2} #{5} #{3 2} #{7} #{2} #{3} #{2 5}])

(defn next-integer-with-factor-in [factors x]
  (apply min (map #(+ x (- % (mod x %))) factors)))

(tests (next-integer-with-factor-in #{2 3 5} 10) := 12)

(tests
 (take-while #(<= % 10) (iterate (partial next-integer-with-factor-in #{2 3}) 2)) := [2 3 4 6 8 9 10])

(defn product-of-integers-in-2-to-m-with-factor-in
  "Find product of only the integers `q` in series 2 to m with `factors`"
  [factors m]
  (apply
   *'
   (take-while #(<= % m) (iterate (partial next-integer-with-factor-in factors) 2))))

(tests (product-of-integers-in-2-to-m-with-factor-in #{2} 4) := 8)

(defn smallest-m-for-x [pfs-series x]
  (let [x-pfs (get pfs-series x)]
    (transduce
     (comp
      (filter ;actual m
       #(when (zero? (mod (product-of-integers-in-2-to-m-with-factor-in x-pfs %) x)) %))
      (take 1))
     +
     (iterate (partial next-integer-with-factor-in x-pfs) (apply max x-pfs))))) ; possible m

(let [pfs-series (seq-of-sets-of-all-prime-factors-for-all-to 10)]
  (tests
   (map (partial smallest-m-for-x pfs-series) (range 2 (inc 10)))  := [2 3 4 5 3 7 4 6 5]))

(defn seq-of-smallest-m-till [p]
  (let [pfs-series (seq-of-sets-of-all-prime-factors-for-all-to p)]
    (map (partial smallest-m-for-x pfs-series) (range 2 (inc p)))))

(tests
 (seq-of-smallest-m-till 10) := [2 3 4 5 3 7 4 6 5])

(defn sum-of-smallest-m-till [p]
  (apply + (seq-of-smallest-m-till p)))

(tests (sum-of-smallest-m-till 100) := 2012)

;; (defn -main [& _args]
;;   (doall (map #(let [n-to %
;;                      p (Math/pow 10 %)]
;;                  (println (str "sum of series to 1e" n-to))
;;                  (println (time (sum-of-smallest-m-till p))))
;;               (range 2 9))))


#_(time (sum-of-smallest-m-till (int (Math/pow 10 6))))