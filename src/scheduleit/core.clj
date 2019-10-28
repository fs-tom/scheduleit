(ns scheduleit.core)


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

;;compute the training intervals for units in the state.
(defn training-intervals [xs])

;;compute the amount of units supplied by t by mission.
(defn supply [xs])


[-1 -1 -1 -1 -1 -1 10] ;;unit 0 is trained by mtt 10 at t=6

