(ns jamiep.test-bed
  (:require [jamiep.euler549 :as naive]
            [jamiep.euler549-v4 :as v4]
            [jamiep.euler549-v5 :as v5]
            [jamiep.euler549-v6 :as v6]
            [jamiep.euler549-v7 :as v7]
            [jamiep.euler549-v9 :as v9]
            [jamiep.euler549-v10 :as v10]
            [jamiep.euler549-v10i :as v10i]))

(defn timeout [timeout-ms callback & args]
  (let [fut (future (apply callback args))
        ret (deref fut timeout-ms ::timed-out)]
    (when (= ret ::timed-out)
      (future-cancel fut))
    ret))

(defn -main [& _args]
  (doseq [p (range 2 9)
          [_ns-alias ns'] (ns-aliases *ns*)
          :let [n-to (long (Math/pow 10 p))]]
    (println (str "sum of series to 1e" n-to))
    (println (time (timeout 1000000000 (ns-resolve ns' 'sum-of-smallest-m-till) p)))))