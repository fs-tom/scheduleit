(ns scheduleit.util)

;;maybe useful...for now we'll stick with vectors though.
;;23% better perf, sadly.
(set! *unchecked-math* true)

(deftype long-range [^long start ^long step ^int count]
  clojure.lang.ISeq
  (first [this] start)
  (next  [this]
    (when (> count 1)
      (.dropFirst this)))
  (seq [this]   (range start (* step count step)))
  clojure.lang.IChunk
  (nth [this  ^int i]
    (if (and (>= i 0) (< i  count))
      (+ start ^long (* i step))
      (throw (ex-info "long-range nth index out of bounds!" {:i i :count count}))))
  (nth [this  i not-found]
    (if (and (>= i 0) (< i count))
      (+ start (* i step))
      not-found))
  (^int count [this] count)
  (^clojure.lang.IChunk dropFirst [this]
   (if (<= count 1)
     (throw (ex-info "dropFirst of empty chunk" {}))
     (long-range.  (+ start step)   step (- count 1))))
  clojure.core.protocols/CollReduce
  (coll-reduce [this f init]
    (loop [x   start
           i   0
           ret init]
      (if (< i count)
        (let [ret (f ret x)]
          (if (reduced? ret)
            ret
            (recur (+ x step)
                   (inc i)
                   ret)))
        ret)))
  (coll-reduce [this f]
    (cond (> count 1)
          (.coll-reduce ^clojure.core.protocols.CollReduce (.dropFirst this)
                         f start)
          (== count 1) start
          :else (f))))

(set! *unchecked-math* false)
