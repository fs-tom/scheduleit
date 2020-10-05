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
  (->solution  (into {} (for [team-count (range team-count)]
                          [team-count {}]))
               (into {} (for [u (range unit-count)]
                          [u {}]))
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
(defn training-intervals [xs])



;;compute the amount of units supplied by t by mission.
(defn supply [xs]
  
  )


[-1 -1 -1 -1 -1 -1 10] ;;unit 0 is trained by mtt 10 at t=6

