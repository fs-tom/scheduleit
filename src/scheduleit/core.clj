;;Re-implementation of the scheduling problem.
(ns scheduleit.core
  (:require [scheduleit.sampledata :as data]))

;;Before we had a MIP to schedule teams to units over time.
;;Now we will schedule via a covering problem and use
;;approximate algorithms to get a solution.


;;Solution representation:
;;A solution must define the assignments of
;;m, u, t
;;so a simple map of assignments by team
;;{team {unit #{t1 t2 t3 t4}}}

;;We can play around with the datastructure too...

;;A 2D array may be useful...

;;       t1 t2  t3  t4
;; M1   nil nil nil u1

;;Given a solution, we have some companion data,
;;namely the demand for training by team over time.

;;The association of teams to demands.

;;The maximum training interval between events.

;;These inform the quality of the solution.

;;A good solution will have

;; All units trained according to <= their maximum training interval

;; All teams will be training 1 unit at a time (for now).

;; The time between team training events will be maximized

;; Teams will train units according to speciality/region [maybe]

;;So we will have multiple objectives.


;;The dumbest solution is a 2d array...

;;We pick random elements in the array and flip them to indicate new assignments.
;;Dimensionality is small enough that we can maybe get away with this approach.

#_
{:assignments []
 :demand      []}

(defrecord solution [teams units actives tmax])

;;basically a bidirectional graph...
(defn init-solution  [tmax team-count unit-count]
  (->solution  (vec (repeat team-count {}))
               (vec (repeat unit-count {}))
               {}
               tmax))

;;maybe a protocol...
(defn assign [^solution sol m u t]
  (let [ms (.teams sol)
        us (.units sol)
        ts (.actives sol)]
    (solution. (assoc-in ms [m t] u)
               (assoc-in us [u t] m)
               (assoc ts t (assoc (ts t {}) m u))
               (.tmax sol))))

;;get a map of {t unit}
(defn assignments
  [^solution sol m]
  (-> m
      ((.teams sol))))

;;get a map of {t team}
(defn training
  [^solution sol u]
  (-> u
      ((.units sol))))

;;compute all training and assignment intersections
(defn events [^solution sol]
  (for [[t mu] (.actives sol)
        [m u]  mu]
    {:time t :mtt m :unit u}))

;;setting up a demand signal.

;;define a move...
;;we can randomly assign a team to a unit at a random
;;time...

;;There are smarter moves to make...
;;Maybe assign to a unit that has unmet demand.
;;We will typically have many units with unmet demand.

;;For now, just be stupid and assign randomly and see what
;;happens....

;;Given a random time, we can assign a unit to an available
;;team, unassign the unit, swap teams if it's already assigned.

;;So one decision axis is to choose random times, then mess with the
;;assignments.  We then grade the assignment at that point in time
;;according to the changes.

;;Some objective functions will be able to determine which objectives were
;;affected piecemeal, and only recompute a delta differential based on
;;the previous objective.

;;dumb solution is to just recompute everything from scratch though.

;;so for now, we stay dumb.

;;let's start with a simple objective...
;;meet all demand.
;;Ensure that, for every time t, we have enough units trained to meet
;;some level of demand for time t....


;;we just need to define our structure and
;;an objective function.

;;decision variables are which mtt to assign to which unit over time.

;;Constraints are
;;  only one MTT assigned per unit per week.

;;  total MTTs trained must equal demand

;;  time between training must be <= training interval for the unit type.


;;so, given a table of assignments
;;assigned(m,u,t)
;;we can derive some information.

;;total units assigned

;;compute the training intervals for units in the state.
;;How much time has elapsed since the previous interval?
;;If we are smart, we can keep the max over sub intervals?
;;We know when the units are active.
(defn training-intervals [^solution sol u]
  )

;;compute the amount of units supplied by t by mission.
(defn supply [xs])

(defn data->solution [{:keys [total-units MTT WKS]}]
  (init-solution WKS MTT total-units))


;;so we have our demand and the solution.
;;Properties of a good solution:

;;Minimal TDY's....

;;That means minimal assignments of
;;MTT's to units.

;;Probably minimal maximum TDY count?
;;  We don't want certain MTTs to have more TDYs
;;  than others.

;;Maximum distance between TDYs

;;Maximum distance between training events for
;;units

;;No TDY's on restricted dates.

;;We must 

;;Fundamental questions:

;;How many trained units do I have at a given time?
;;How many demands for trained units of a type do I have?


;;Do my trained units exceed my demands?
;;  What are my demands?

;;A rambling way to get from the encoded demand signal of
;;[[type year] quantity] into a vector indexed by
;;week, where the keys correspond to type, vals are quantity.
;;Adds a redundant :week key for readability.
(defn render-demand [{:keys [msn-demand]}]
  (->> (for [[[demand-type year] quantity] (sort-by (comp second key) msn-demand)
             week (range (* year 52) (* (inc year) 52))]
         [week {:type demand-type :quantity quantity :year year}])
       (group-by first)
       (sort-by key)
       (reduce (fn [acc [week xs]]
                 (->> xs
                      (sort-by (comp :type second))
                      (reduce (fn [m [_ {:keys [type quantity]}]]
                                (assoc m type quantity)) {:week week})
                      (conj acc))) [])))


;;Since demands vary by type, we have multiple trained units by type, so
;;we really only need to compare like populations (they aren't interchangeable).

;;May even have sub problems....hmm.  Perhaps a hierarchical objective function
;;where we solve the mission training requirements first, then solve the rest?


;;naive way to compute available for each day is to reduce over all units by type
;;We need to know who as been trained.  How do we know if a unit is trained on day
;;t?  If it's wait time is positive, or the time since last event is < the
;;training interval type.  If we train a unit, we can register the interval
;;it will be trained for based on its training interval type.
;;Assigning a mtt to train a unit will also update the unit's availability.
;;So total availability at any given time is finding the intersecting unit
;;availability samples for that time.
(defn compute-available [s d]
  (let [utype  (d :unit-type)
        itype  (d :interval-type)]
    (reduce (fn [acc unit]
              (let [interval (-> unit utype itype)])))
    ))

(defn intervals [xs]
  (reduce (fn [acc [l r]]
            (assoc acc l r)) (sorted-map) xs))

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

;;slow but portable version...
#_
(defn intersection [sm k]
  (when-let [ab (first (rsubseq sm <= k))]
    (when (< k (val ab))
      ab)))

;;fast but not portable version...
;;doing naive o(N) over some intervals (as vecs at least)
;;appears slower.
(defn intersection [sm k]
  (when-let [ab (when-let [xs (.seqFrom ^clojure.lang.PersistentTreeMap sm k false)]
                  (.first ^clojure.lang.ISeq xs))]
    (when (<= k (.getValue ^java.util.Map$Entry ab))
      ab)))

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

