(ns scheduleit.util
  (:require [clojure.data.avl :as avl]))

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



;;in lieu of data.avl, we can use these for now.
;;tim pratley from
;;https://stackoverflow.com/questions/1981859/finding-keys-closest-to-a-given-value-for-clojure-sorted-maps
;;Revised to allow for intervallic searching...
(defn abs [x] (if (neg? x) (- x) x))
#_
(defn find-closest [sm k]
  (if-let [ab (first (rsubseq sm <= k))]
    (let [a (key ab)]
      (if (= a k)
        ab
        (if-let [bc  (first (subseq sm >= k))]
          (let [b (key bc)]
            (if (< (abs (- k b)) (abs (- k a)))
              bc
            ab)))))
    (first (subseq sm >= k))))



(set! *unchecked-math* true)
(defn fast-intersection [sm k]
  (when-let [ab (when-let [xs (.seqFrom ^clojure.lang.PersistentTreeMap sm k false)]
                  (.first ^clojure.lang.ISeq xs))]
    (when (<= ^long k ^long (.getValue ^java.util.Map$Entry ab))
      ab)))
(set! *unchecked-math* false)

;;clojure.data.avl is pretty good though, and should be
;;portable to cljs...
(defn fast-avl-intersection [sm k]
  (when-let [ab (avl/nearest sm <= k)]
    (when (<= k (.val ^clojure.lang.MapEntry ab))
      ab)))

(comment ;;profiling

(require '[criterium.core :as c])
(require '[clojure.data.avl :as avl])

(def samples (sorted-map 10 20 35 40 50 60))
(def avl-samples (avl/sorted-map 10 20 35 40 50 60))

;;slow but portable version...
(defn slow-intersection [sm k]
  (when-let [ab (first (rsubseq sm <= k))]
    (when (<= k (val ab))
      ab)))
;;user> (c/quick-bench (slow-intersection samples 10))
;;Execution time mean : 1.142835 µs

;;fast but not portable version...
;;doing naive o(N) over some intervals (as vecs at least)
;;appears slower.
(defn intersection [sm k]
  (when-let [ab (when-let [xs (.seqFrom ^clojure.lang.PersistentTreeMap sm k false)]
                  (.first ^clojure.lang.ISeq xs))]
    (when (<= k (.getValue ^java.util.Map$Entry ab))
      ab)))

;;user> (c/quick-bench (intersection samples 10))
;;Execution time mean : 112.924740 ns

(set! *unchecked-math* true)
(defn fast-intersection [sm k]
  (when-let [ab (when-let [xs (.seqFrom ^clojure.lang.PersistentTreeMap sm k false)]
                  (.first ^clojure.lang.ISeq xs))]
    (when (<= ^long k ^long (.getValue ^java.util.Map$Entry ab))
      ab)))
(set! *unchecked-math* false)
;;user> (c/quick-bench (fast-intersection samples 10))
;;Execution time mean : 91.716445 ns


(defn avl-intersection [sm k]
  (when-let [ab (avl/nearest sm <= k)]
    (when (<= k (val ab))
      ab)))
;;Execution time mean : 163.168134 ns

(defn fast-avl-intersection [sm k]
  (when-let [ab (avl/nearest sm <= k)]
    (when (<= k (.val ^clojure.lang.MapEntry ab))
      ab)))
;;Execution time mean : 156.294912 ns

(set! *unchecked-math* true)
(defn fastest-avl-intersection [sm k]
  (when-let [ab (avl/nearest sm <= k)]
    (when (<= ^long k ^long (.val ^clojure.lang.MapEntry ab))
      ab)))
(set! *unchecked-math* false)
;; user> (c/quick-bench (fastest-avl-intersection avl-samples 10))
;; Execution time mean : 142.348268 ns

;;substantially slower, but "may" be faster on cljs...
;; (set! *unchecked-math* true)
;; (defn raw-intersection [intervals ^long k]
;;   (reduce (fn [acc ^clojure.lang.Indexed lr]
;;             (if (>= k ^long (.nth lr 0))
;;               (if     (<= k ^long (.nth lr 1))
;;                 (reduced lr)
;;                 acc)
;;               acc)) nil intervals))
;; (set! *unchecked-math* false)
)

(defn do-range! [^longs arr ^long from ^long to f]
  (let [bound (unchecked-inc to)]
    (loop [idx from]
      (if (< idx bound)
        (do (aset arr idx ^long (f (aget arr idx)))
            (recur (unchecked-inc idx)))))
    arr))

(defn do-vrange! [v ^long from ^long to f]
  (let [bound (unchecked-inc to)]
    (loop [idx from
           ^clojure.lang.ITransientVector acc v]
      (if (< idx bound)
        (recur (unchecked-inc idx)
               (.assocN acc idx (f (.nth acc idx))))
        acc))))

(set! *unchecked-math* true)
(defn cow-update [^longs arr ^long idx ^long v]
  (let [^longs res (Arrays/copyOf arr (alength arr))]
    (aset res idx v)
    res))
(set! *unchecked-math* false)

(defn memo-1 [f]
  (let [^java.util.HashMap tbl (java.util.HashMap.)]
    (fn [k] (if-let [res (.get tbl k)]
              res
              (let [res (f k)]
                (do (.put tbl k res)
                    res))))))
