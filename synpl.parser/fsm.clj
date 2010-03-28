
(ns synpl.fsm
  (:use clojure.contrib.duck-streams)
  (:use clojure.contrib.pprint))

;; playing with fsms

(defstruct fsm :states :edges :input :fail :success)

(defstruct state :label)

(defstruct edge :from :to :action :condition)

(defn dump-as-dot [fsm]
  (println "digraph Test {")
  ;; dump nodes
  (doseq [state (:states fsm)]
    (let [state-label (:label state)
          fill-color (cond (= (:input fsm) state-label) "yellow"
                           (= (:fail fsm) state-label) "red"
                           (= (:success fsm) state-label) "green"
                           :else "gray")]
      (println (format "  %s [label=\"%s\",style=\"filled\",color=\"%s\"];"
                       state-label
                       state-label
                       fill-color))))
  (println)
  ;; dump edges
  (doseq [edge (:edges fsm)]
    (println (format "  %s -> %s [label=\"%s\\n%s\"];"
                     (:from edge)
                     (:to edge)
                     (:condition edge)
                     (:action edge))))
  (println "};"))

(def counter (atom 0))

(defn get-unique [prefix]
  (swap! counter inc)
  (str prefix @counter))

(defn make-fsm []
  (let [ala (get-unique "ala")
        bala (get-unique "bala")
        portocala (get-unique "portocala")
        state-labels [ala bala portocala]
        edge-labels [[ala bala]
                     [ala portocala]]
        states (map (fn [state-label] (struct state state-label))
                    state-labels)
        edges (map (fn [[from to]] (struct edge from to "action" "condition"))
                   edge-labels)]
    (struct fsm (vec states) (vec edges) ala bala portocala)))

(defn input-edges [fsm state]
  (let [state-label (:label state)]
    (filter (fn [edge] (= state-label (:to edge)))
            (:edges fsm))))

(defn output-edges [fsm state]
  (let [state-label (:label state)]
    (filter (fn [edge] (= state-label (:from edge)))
            (:edges fsm))))

(defn to-simplify [fsm]
  (filter (fn [state] (= 1 (count (output-edges fsm state))))
          (:states fsm)))

(defn remove-state [afsm state]
  (let [in-edges (input-edges afsm state)
        out-edge (first (output-edges afsm state))
        new-edges (map (fn [in-edge] (struct edge
                                             (:from in-edge)
                                             (:to out-edge)
                                             "action"
                                             "condition"))
                       in-edges)
        state-label (:label state)]
    (struct fsm
            (vec (filter (fn [itr-state] (not (= (:label itr-state)
                                                 state-label)))
                         (:states afsm)))
            (vec (concat new-edges
                         (filter (fn [itr-edge] (and (not (= state-label (:to itr-edge)))
                                                     (not (= state-label (:from itr-edge)))))
                                 (:edges afsm))))
            (:input afsm)
            (:fail afsm)
            (:success afsm))))

(defn simplify [fsm]
  (loop [result fsm
         to-remove (to-simplify fsm)]
    (if (empty? to-remove)
      result
      (recur (remove-state result (first to-remove))
             (rest to-remove)))))

;; connect the success node to the input of the next fsm
;; both fails now point to a new common fail state
(defn sequence-fsm [fsm1 fsm2]
  (let [fail-label (get-unique "fail")
        states (cons (struct state fail-label)
                     (concat (:states fsm1) (:states fsm2)))
        new-edges [(struct edge (:fail fsm1) fail-label "action" "condition")
                   (struct edge (:fail fsm2) fail-label "action" "condition")
                   (struct edge (:success fsm1) (:input fsm2) "action" "condition")]
        edges (concat new-edges (:edges fsm1) (:edges fsm2))]
    (simplify (struct fsm
                      (vec states)
                      (vec edges)
                      (:input fsm1)
                      fail-label
                      (:success fsm2)))))

;; kind of 'transposed sequence'
;; connects the failure to the input of the next fsm
;; creates a new common success node
(defn alternative-fsm [fsm1 fsm2]
  (let [success-label (get-unique "success")
        states (cons (struct state success-label)
                     (concat (:states fsm1) (:states fsm2)))
        new-edges [(struct edge (:fail fsm1) (:input fsm2) "action" "condition")
                   (struct edge (:success fsm1) success-label "action" "condition")
                   (struct edge (:success fsm2) success-label "action" "condition")]
        edges (concat new-edges (:edges fsm1) (:edges fsm2))]
    (simplify (struct fsm
                      (vec states)
                      (vec edges)
                      (:input fsm1)
                      (:fail fsm2)
                      success-label))))

;; many-fsm
;; loops until failure of the enclosed fsm, at which point it succeeds.
(defn many-fsm [afsm]
  (let [loop-edge (struct edge (:success afsm) (:input afsm) "action" "condition")
        new-edges (vec (cons loop-edge (:edges afsm)))]
    (struct fsm (:states afsm) new-edges (:input afsm) nil (:fail afsm))))

;; many1-fsm - as sequence of fsm and many-fsm

(defn reset-counter []
  (swap! counter (fn [value] 0)))

(defn to-file [filename fsm]
  (spit filename
        (with-out-str (dump-as-dot fsm))))

(defn sequence-n [n]
  (let [fsms (map (fn [_] (make-fsm)) (range n))]
    (reduce sequence-fsm fsms)))