(ns jamiep.euler549-v4)

(defn to-the-power [factor exp]
  (reduce * 1 (repeat exp factor)))

(defn a-prime-factor-for-all-to
  ([max-n]
   (loop [pfs (apply vector-of :int (range 0 (inc max-n)))
          n 2]
     (if (<= (to-the-power n 2) max-n)
       (let [new-pfs (if (= n (get pfs n))
                       (reduce #(assoc %1 %2 n)
                               pfs
                               (range (* n 2) (inc max-n) n))     ; found new prime
                       pfs)]
         (recur new-pfs (inc n)))
       pfs))))

(defn prime-factors-of [pre-calculated-pfs x]
  (loop [to-factorise x
         factors '()]
    (let [factor (get pre-calculated-pfs to-factorise)]
      (if (= to-factorise 1)
        factors
        (recur (/ to-factorise factor) (conj factors factor))))))

(defn smallest-m-for-power-of-factor
  "Find m where m! is divisible by pf ^ power.
  m! is made up of factors 1..m but we are only interested in those factors that are multiples of pf."
  [pf power]
  (loop [m-to-try pf]
    (let [product-of-multiples-of-pf (apply * (range pf (inc m-to-try) pf))]
      ;instead of seeing if (!m-to-try) is divisible by the pf ^ power, just look at the product of the
      ; multiples of `pf` up to m-to-try.
      (if (zero? (mod product-of-multiples-of-pf (to-the-power pf power)))
        m-to-try
        (recur (+ m-to-try pf))))))

(defn smallest-m-for-primes [f-pf-n]
  (apply max (map (fn [[factor freq]] (smallest-m-for-power-of-factor factor freq)) f-pf-n)))


(defn seq-of-smallest-m-till-p [p]
  (let [pfs (a-prime-factor-for-all-to p)]
    (->>
     (range 2 (inc p))
     (map (fn [x] (frequencies (prime-factors-of pfs x))))
     (map smallest-m-for-primes))))


(defn -main [& _args]
  (doall (map #(let [n-to %
                     p (to-the-power 10 %)]
                 (println (str "sum of series to 1e" n-to))
                 (println (time (apply + (seq-of-smallest-m-till-p p)))))
              (range 2 9))))

;(defn -main [& args]
;  (doall (map #(let [p (x-to-power-n 10 %)]
;                 (do (println "sum of prime factor series to " p)
;                     (println (time (seq-of-smallest-m-till-p p)))))
;              (range 2 8))))

;(defn -main [& args]
;  (println (time (apply + (sum-of-smallest-m-till-p (int 1e2))))))