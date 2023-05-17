(ns jamiep.euler549-v4
  (:require [hyperfiddle.rcf :refer [tests]]))

(defn a-prime-factor-for-all-to [max-n]
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

(defn prime-factors-of [pre-calculated-pfs x]
  (loop [to-factorise x
         factors (transient [])]
    (let [factor (get pre-calculated-pfs to-factorise)]
      (if (= to-factorise 1)
        (persistent! factors)
        (recur (quot to-factorise factor) (conj! factors factor))))))

; unfortunately this seems to be slower.
#_(defn prime-factors-of [pfs x]
  (let [quotients (->> (iterate #(/ % (pfs %)) x)
                       (take-while #(not= % 1)))]
    (into [(last quotients)]
          (map (partial apply /)
               (partition 2 1 quotients)))))

(defn smallest-m-for-power-of-factor
  "Find m where m! is divisible by pf ^ power.
  m! is made up of factors 1..m but we are only interested in those factors that are multiples of pf."
  [pf power]
  (loop [m-to-try pf]
    (let [product-of-multiples-of-pf (apply * (range pf (inc m-to-try) pf))]
      ;instead of seeing if (!m-to-try) is divisible by the pf ^ power, just look at the product of the
      ; multiples of `pf` up to m-to-try.
      (if (zero? (mod product-of-multiples-of-pf (Math/pow pf power)))
        m-to-try
        (recur (+ m-to-try pf))))))

(defn smallest-m-for-primes [f-pf-n]
  (apply max (map (fn [[factor freq]] (smallest-m-for-power-of-factor factor freq)) f-pf-n)))

(defn pfs-to-smallest-m-till [p]
  (let [pfs (a-prime-factor-for-all-to p)]
    (map (comp smallest-m-for-primes frequencies (partial prime-factors-of pfs)))))

(defn seq-of-smallest-m-till [p]
  (transduce (pfs-to-smallest-m-till p) conj (range 2 (inc p))))

(defn sum-of-smallest-m-till [p]
  (transduce (pfs-to-smallest-m-till p) + (range 2 (inc p))))

(tests
 (seq-of-smallest-m-till 10) := [2 3 4 5 3 7 4 6 5]
 (sum-of-smallest-m-till 100) := 2012)

(defn -main [& _args]
  (doall (map #(let [n-to %
                     p (Math/pow 10 %)]
                 (println (str "sum of series to 1e" n-to))
                 (println (time (sum-of-smallest-m-till p))))
              (range 2 9))))


#_(time (sum-of-smallest-m-till (int (Math/pow 10 6))))
