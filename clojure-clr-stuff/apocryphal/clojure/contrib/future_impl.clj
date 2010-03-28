
(ns apocryphal.clojure.contrib.future-impl
  (:import System.Threading.ThreadPool
           System.Threading.WaitCallback
           System.Threading.AutoResetEvent))

(defn future-sharp
  "This is very similar to Clojure's future, but it doesn't return a
  ref, but a function. Instead of `deref`ing, call the function.

  This version uses a ThreadPool work item.

  It also uses an AutoResetEvent to signal work completion. The
  Set/Close hack seems to work :-)"
  [f]
  (let [result (atom {:computed false :result nil :error nil})
        waitHandle (AutoResetEvent. false)
        t (ThreadPool/QueueUserWorkItem
           (gen-delegate WaitCallback [_]
                         (try 
                          (let [computation-result (f)]
                            (swap! result (constantly {:computed true
                                                       :result computation-result
                                                       :error nil})))
                          (catch Exception ex
                            (swap! result (constantly {:computed true
                                                       :result nil
                                                       :error ex})))
                          (finally 
                           (.Set waitHandle)
                           (.Close waitHandle)))))]
    (fn []
      (if (:computed @result)
        (if (:error @result)
          (throw (Exception. "Future processing failed."
                             (:error @result)))
          (:result @result))
        (do
          (.WaitOne waitHandle)
          (recur))))))

;; The functions below are shamelessly extracted from clojure.core.
;; They are modified to use future-sharp instead of future.

(defn pmap
  "Like map, except f is applied in parallel. Semi-lazy in that the
  parallel computation stays ahead of the consumption, but doesn't
  realize the entire result unless required. Only useful for
  computationally intensive functions where the time of f dominates
  the coordination overhead."
  ([f coll]
   (let [n (+ 2 Environment/ProcessorCount)
         rets (map #(future-sharp (fn [] (f %))) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (x) (step xs (rest s)))
                   (map (fn [f] (f)) vs))))]
     (step rets (drop n rets))))
  ([f coll & colls]
   (let [step (fn step [cs]
                (lazy-seq
                 (let [ss (map seq cs)]
                   (when (every? identity ss)
                     (cons (map first ss) (step (map rest ss)))))))]
     (pmap #(apply f %) (step (cons coll colls))))))

(defn pcalls
  "Executes the no-arg fns in parallel, returning a lazy sequence of
  their values"
  [& fns] (pmap #(%) fns))

(defmacro pvalues
  "Returns a lazy sequence of the values of the exprs, which are
  evaluated in parallel"
  [& exprs]
  `(pcalls ~@(map #(list `fn [] %) exprs)))
