(ns parse-markdown.utils
  (:use clojure.java.io))

(defn lazy-reader [filename]
  (let [rd (fn [rdr]
             (let [buf (char-array 4096)
                   n (.read rdr buf 0 4096)]
               (condp == n
                 -1 (.close rdr)
                 0 (recur rdr)
                 (take n buf))))
        lr (fn lr [rdr]
             (lazy-seq
               (if-let [b (rd rdr)]
                 (concat b (lr rdr)))))]
    (lr (reader filename))))
