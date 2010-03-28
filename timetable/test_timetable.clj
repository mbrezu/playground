
(ns timetable.test-timetable
  (:use timetable.timetable)
  (:use beak-check.beak-check)
  (:use clojure.contrib.test-is))

(defn equal-to [value]
  (fn [data-structure]
    (= data-structure value)))

(defn value-options [& options]
  (check-all keyword?
             (apply one-of (map (fn [option] (equal-to option))
                                options))))

(def is-timeslot (check-all (is-struct <timeslot>)
                            (with-values-for
                              :day (instance-of Integer)
                              :interval (instance-of Integer))))

(def is-resource (check-all (is-struct <resource>)
                            (with-values-for
                              :id keyword?
                              :type (value-options :prof :room :class)
                              :available-timeslots (set-of is-timeslot)
                              :constraints (sequence-of fn?)
                              :score (instance-of Integer)
                              :externally-scheduled (set-of is-timeslot))))

(def is-unscheduled-activity (check-all (is-struct <unscheduled-activity>)
                                        (with-values-for
                                          :class keyword?
                                          :prof keyword?)))

(def is-scheduled-activity (check-all (is-struct <scheduled-activity>)
                                      (with-values-for
                                        :class keyword?
                                        :prof keyword?
                                        :room keyword?
                                        :timeslot is-timeslot)))

(def is-schedule (check-all (is-struct <schedule>)
                            (with-values-for
                              :scheduled-activities (sequence-of is-scheduled-activity)
                              :unscheduled-activities (sequence-of is-unscheduled-activity)
                              :resources (map-of is-resource)
                              :score (instance-of Integer))))

(def default-schedule
     (set (for [day (range 1 11) interval (range 1 9)]
            (struct <timeslot> day interval))))

(defn constraint-too-many-activities [max-number penalty]
  (fn [resource-daily-activities]
    (apply + (for [[k v] resource-daily-activities]
               (let [num-activities (count v)]
                 (if (> num-activities max-number) (* (- max-number num-activities) penalty) 0))))))

(defn constraint-too-few-activities [min-number penalty]
  (fn [resource-daily-activities]
    (apply + (for [[k v] resource-daily-activities]
               (let [num-activities (count v)]
                 (if (and (> num-activities 0)
                          (< num-activities min-number))
                   (* (- num-activities min-number) penalty) 0))))))

(defn constraint-windows [penalty]
  (fn [resource-daily-activities]
    (apply + (for [[k v] resource-daily-activities]
               (let [intervals (sort (map #(-> % :timeslot :interval) v))
                     total-window-size (apply + (map #(- %1 %2 1)
                                                     (rest intervals)
                                                     intervals))]
                 (* total-window-size (- penalty)))))))

(defn make-room [id]
  (struct <resource>
          (keyword id)
          :room
          default-schedule
          [(constraint-too-few-activities 3 5)]
          0
          #{}))

(defn make-class [id]
  (struct <resource>
          (keyword id)
          :class
          default-schedule
          [(constraint-too-many-activities 4 10)
           (constraint-too-few-activities 2 10)
           (constraint-windows 20)]
          0
          #{}))

(defn make-prof [id]
  (struct <resource>
          (keyword id)
          :prof
          default-schedule
          [(constraint-too-many-activities 7 10)
           (constraint-too-few-activities 4 10)
           (constraint-windows 10)]
          0
          #{}))

(defn make-profs []
  (for [i (range 1 11)] (make-prof (str "prof_" i))))

(defn make-classes []
  (for [i (range 1 21)] (make-class (str "class_" i))))

(defn make-rooms []
  (for [i (range 1 21)] (make-room (str "room_" i))))

(defn make-unscheduled-activities [profs classes]
  (for [prof (range (count profs))
        class (range (count classes))
        i (range 2)]
    (struct <unscheduled-activity>
            (-> classes (nth  class) :id)
            (-> profs (nth prof) :id))))

(defn remove-mornings [resource]
  (let [remaining-timeslots (set (filter (fn [ts] (> (:interval ts) 4))
                                         (:available-timeslots resource)))
        externally-scheduled (clojure.set/difference (:available-timeslots resource)
                                                     remaining-timeslots)]
    (-> resource
        (update-in [:available-timeslots] (fn [_] remaining-timeslots))
        (update-in [:externally-scheduled] (fn [_] externally-scheduled)))))

(defn change-schedule [schedule]
  (update-in schedule
             [:resources :class_7]
             remove-mornings))

(def test-schedule
     (let [profs (make-profs)
           classes (make-classes)
           rooms (make-rooms)
           unscheduled-activities (make-unscheduled-activities profs classes)
           all-resources (concat profs classes rooms)
           resources-by-id (apply hash-map (interleave (map :id all-resources)
                                                       all-resources))
           schedule (struct <schedule>
                            []
                            unscheduled-activities
                            resources-by-id
                            0)]
       (-> schedule
           change-schedule
           sort-unscheduled-activites)))

(deftest test-makers
  (is (= true (is-resource (make-room "first_room"))))
  (is (= true (is-resource (make-prof "first_prof"))))
  (is (= true (is-resource (make-class "first_class")))))

(deftest test-test-schedule
  (is (= true (is-schedule test-schedule)))
  (is (= true (schedule-is-possible test-schedule)))
  (is (-> test-schedule :unscheduled-activities first :class .toString (.endsWith "_7"))))

(deftest test-allocate-first
  (let [variations (allocate-first test-schedule)]
    (is (= true ((sequence-of is-schedule) (take 5 variations))))))

(deftest test-by-validate-schedule
  (let [schedule (allocate-n test-schedule 20)]
    (is (validate-schedule schedule default-schedule))))

(deftest test-unschedule-percent-of-activities
  (let [schedule (allocate-n test-schedule 20)
        updated-shedule (unschedule-percent-of-activities schedule 0.5)]
    (is (validate-schedule updated-shedule default-schedule))
    (is (= 10 (-> updated-shedule :scheduled-activities count)))))

(run-tests 'timetable.test-timetable)
