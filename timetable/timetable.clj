
;; TODO
;; * reallocation with hill-climbing
;; * allocation should pick at random from the schedules with the best score
;; * parallelize allocate-first? (if there are many available timeslots)
;; * load/save file
;; * GUI to edit schedule inputs
;; * GUI to provide schedules for a given resource
;; * names for profs
;; * subjects associated with profs?
;; * more constraints (and also associated with profs and rooms)
;; * distance between rooms (organize rooms in groups?)

(ns timetable.timetable
  (:use clojure.contrib.pprint)
  (:use clojure.contrib.seq-utils))

(defstruct <resource>
  :id
  :type
  :available-timeslots
  :constraints
  :score
  :externally-scheduled)

(defstruct <timeslot> :day :interval)

(defstruct <unscheduled-activity> :class :prof)

(defstruct <scheduled-activity> :class :prof :room :timeslot)

(defstruct <schedule> :scheduled-activities :unscheduled-activities :resources :score)

(defn get-timeslots [schedule {prof :prof class :class :as us-activity}]
  (let [prof-timeslots (-> schedule :resources prof :available-timeslots)
        class-timeslots (-> schedule :resources class :available-timeslots)
        common-timeslots (clojure.set/intersection prof-timeslots class-timeslots)]
    common-timeslots))

(defn sort-unscheduled-activites [schedule]
  (let [unscheduled-activities (:unscheduled-activities schedule)
        uas-with-timeslots (map (fn [ua] [(count (get-timeslots schedule ua)) ua])
                                unscheduled-activities)
        sorted-uas-with-timeslots (sort (fn [ua1 ua2] (- (first ua1) (first ua2)))
                                        uas-with-timeslots)
        sorted-uas (map (fn [ua] (nth ua 1)) sorted-uas-with-timeslots)]
    (assoc-in schedule
              [:unscheduled-activities]
              sorted-uas)))

(defn free-rooms [schedule timeslot]
  (let [rooms (filter #(= (:type %) :room) (vals (:resources schedule)))
        available-rooms (filter #((:available-timeslots %) timeslot ) rooms)]
    available-rooms))

(defn allocate-time-slot [schedule resource-id timeslot]
  (update-in schedule
             [:resources resource-id :available-timeslots]
             (fn [available-timeslots] (disj available-timeslots timeslot))))

(defn deallocate-time-slot [schedule resource-id timeslot]
  (update-in schedule
             [:resources resource-id :available-timeslots]
             (fn [available-timeslots] (conj available-timeslots timeslot))))

(defn get-resource-activities [schedule resource-id]
  (filter (fn [activity] (or (= (-> activity :prof)
                                resource-id)
                             (= (-> activity :class)
                                resource-id)
                             (= (-> activity :room)
                                resource-id)))
          (:scheduled-activities schedule)))

(defn daily-activities [schedule resource-id]
  (let [resource-activities (get-resource-activities schedule resource-id)]
    (group-by #(-> % :timeslot :day) resource-activities)))

(defn score-resource [schedule resource-id]
  (let [activities-by-day (daily-activities schedule resource-id)]
    (apply + (for [constraint (-> schedule :resources resource-id :constraints)]
               (constraint activities-by-day)))))

(defn get-affected-resource-ids [scheduled-activity]
  [(:class scheduled-activity)
   (:prof scheduled-activity)
   (:room scheduled-activity)])

(defn update-resource-scores [schedule resource-ids]
  (reduce (fn [schedule resource-id]
             (assoc-in schedule
                       [:resources resource-id :score]
                       (score-resource schedule resource-id)))
          schedule
          resource-ids))

(defn update-schedule-score [schedule newly-scheduled-activity]
  (let [affected-resource-ids (get-affected-resource-ids newly-scheduled-activity)
        get-score-for-resources (fn [schedule resource-ids]
                                  (apply + (map #(-> schedule :resources % :score)
                                                resource-ids)))
        old-score-for-resources (get-score-for-resources schedule affected-resource-ids)
        updated-schedule (update-resource-scores schedule affected-resource-ids)
        new-score-for-resources (get-score-for-resources updated-schedule
                                                         affected-resource-ids)
        score-delta (- new-score-for-resources old-score-for-resources)]
    (update-in updated-schedule [:score] #(+ % score-delta))))

(defn allocate-first [schedule]
  (let [unscheduled-activities (:unscheduled-activities schedule)
        activity (first unscheduled-activities)
        timeslots (get-timeslots schedule activity)]
    (for [timeslot timeslots
          room (take 3 (shuffle (free-rooms schedule timeslot)))]
      (let [prof-id (:prof activity)
            class-id (:class activity)
            room-id (:id room)
            scheduled-activity (struct <scheduled-activity>
                                       class-id
                                       prof-id
                                       room-id
                                       timeslot)
            updated-schedule (-> schedule
                                 (allocate-time-slot prof-id timeslot)
                                 (allocate-time-slot class-id timeslot)
                                 (allocate-time-slot room-id timeslot))
            unscored-schedule (struct <schedule>
                                      (conj (:scheduled-activities updated-schedule)
                                            scheduled-activity)
                                      (rest (:unscheduled-activities updated-schedule))
                                      (:resources updated-schedule)
                                      (:score schedule))
            scored-schedule (update-schedule-score unscored-schedule scheduled-activity)]
        scored-schedule))))

(defn allocate-n [schedule n]
  (if (= n 0)
    schedule
    (let [alternatives (allocate-first schedule)
          best-alternative (apply max-key :score alternatives)]
      (recur best-alternative (dec n)))))

(defn- enough-timeslots-for-profs-and-classes [schedule]
  (reduce #(and %1 %2)
          true
          (for [resource-id (-> schedule :resources keys)]
            (let [resource-activities (filter (fn [ua] (or (= (:class ua) resource-id)
                                                           (= (:prof ua) resource-id)))
                                              (-> schedule :unscheduled-activities))
                  resource-available-timeslots (-> schedule
                                                   :resources
                                                   resource-id
                                                   :available-timeslots)]
              (< (count resource-activities) (count resource-available-timeslots))))))

(defn- enough-timeslots-for-rooms [schedule]
  (let [total-room-timeslots (reduce + 0 (map #(-> % :available-timeslots count)
                                              (filter #(= :room (:type %))
                                                      (-> schedule :resources vals))))]
    (< (-> schedule :unscheduled-activities count)
       total-room-timeslots)))

(defn schedule-is-possible [schedule]
  (and (enough-timeslots-for-profs-and-classes schedule)
       (enough-timeslots-for-rooms schedule)))

(defn- check-repetitions
  ([items]
     (check-repetitions items #{}))
  ([items acc]
     (if (empty? items)
       false
       (if (acc (first items))
         true
         (recur (rest items) (conj acc (first items)))))))

(defn- validate-resource-schedule [schedule resource-id expected-all-timeslots]
  (let [resource-activities (get-resource-activities schedule resource-id)
        resource-activities-timeslots (map :timeslot resource-activities)
        has-repetitions (check-repetitions resource-activities-timeslots)]
    (if has-repetitions
      false
      (let [resource (-> schedule :resources resource-id)
            available-timeslots (:available-timeslots resource)
            externally-scheduled (:externally-scheduled resource)
            actual-all-timeslots (clojure.set/union (set resource-activities-timeslots)
                                                    available-timeslots
                                                    externally-scheduled)]
        (= actual-all-timeslots expected-all-timeslots)))))

(defn- validate-scoring [schedule]
  (let [resource-scores (reduce + 0 (map :score (-> schedule :resources vals)))]
    (= resource-scores (:score schedule))))

(defn validate-schedule [schedule expected-all-timeslots]
  (and
   (validate-scoring schedule)
   (reduce #(and %1 %2)
           true
           (for [resource-id (-> schedule :resources keys)]
             (validate-resource-schedule schedule resource-id expected-all-timeslots)))))

;; Note: does not remove the scheduled activity from the list of
;; scheduled activities; that is the job of the caller; performs all
;; other necessary updates.
(defn unschedule-activity [schedule scheduled-activity]
  (let [{:keys [prof class room timeslot]} scheduled-activity
        unscheduled-activity (struct <unscheduled-activity> class prof)
        updated-schedule (-> schedule
                             (deallocate-time-slot prof timeslot)
                             (deallocate-time-slot class timeslot)
                             (deallocate-time-slot room timeslot)
                             (update-in [:unscheduled-activities]
                                        (fn [uas] (conj uas unscheduled-activity))))]
    updated-schedule))

(defn unschedule-percent-of-activities [schedule percent]
  (let [num-sas-to-remove (int (* percent (-> schedule :scheduled-activities count)))
        shuffled-sas (-> schedule :scheduled-activities shuffle)
        to-drop (take num-sas-to-remove shuffled-sas)
        to-keep (drop num-sas-to-remove shuffled-sas)
        updated-schedule (reduce (fn [schedule activity]
                                   (unschedule-activity schedule activity))
                                 (assoc-in schedule [:scheduled-activities] to-keep)
                                 to-drop)]
    updated-schedule))

(defn allocate-completely [schedule]
  (let [uas (-> schedule :unscheduled-activities count)]
    (if (= uas 0)
      schedule
      (let [alternatives (allocate-first schedule)
            best-alternative (apply max-key :score alternatives)]
        (if (nil? best-alternative)
          best-alternative
          (recur best-alternative))))))

(defn allocate-and-improve [schedule max-iterations unschedule-percent]
  (loop [iteration 0
         to-improve (allocate-completely schedule)]
    (let [deallocated (unschedule-percent-of-activities to-improve unschedule-percent)
          reallocated (allocate-completely deallocated)]
      (println (:score to-improve) (:score reallocated))
      (if (nil? reallocated)
        to-improve
        (if (> (:score reallocated) (:score to-improve))
          (do
            (println "new max")
            (recur 0 reallocated))
          (if (= (inc iteration) max-iterations)
            to-improve
            (recur (inc iteration) to-improve)))))))