(ns drt-clj.core
  (:gen-class))

(require '[clj-time.core :as t])
(require '[clj-time.format :as f])
(require '[clj-time.coerce :as c])

(def flight {:code "BA0001" :scheduled "2016-10-31T07:20:05.000Z" :pax 90})

(def paxDisRate 25)

(defn paxMinutes
  [pax rate]
  (reverse (conj (repeat (int (/ pax rate)) rate) (mod pax rate))))

(paxMinutes 99 25)

;; use reduce to create time in vector?

(paxByMinute (flight :pax) paxDisRate (flight :scheduled))

(c/to-long (f/parse (flight :scheduled)))

(count '(1 2))

(def splitRatios [
                   {:paxType :eea-mr :queue :eea-desk :ratio 0.6}
                   {:paxType :eea-mr :queue :e-gate :ratio 0.2}
                   {:paxType :eea-nmr :queue :eea-desk :ratio 0.2}
                   ])

(def procTimes
  {
    :eea-mr {:eea-desk 0.25 :e-gate 0.2}
    :eea-nmr {:eea-desk 0.75}
    :visa {:non-eea-desk 0.5 :fast-track 0.25}
    :non-visa {:non-eea-desk 0.8 :fast-track 0.3}})

(defn paxProcTime
  [paxType queue]
  (get (get procTimes paxType) queue))

(defn splitPax
  [pax-queue-splits pax-mins]
  (map (fn [pax]
         (into []
               (map (fn [split]
                      {:paxType (:paxType split) :queue (:queue split) :pax (* (:ratio split) pax)})
                    pax-queue-splits)))
       pax-mins))



(def myPaxLoads (splitPax splitRatios (paxByMinute (flight :pax) paxDisRate)))

myPaxLoads

(defn wlplimpl
  [paxLoads f]
  (map
    (fn [splits]
      (reduce
        (fn [a {paxType :paxType queue :queue pax :pax}]
          (assoc a queue (+ (or (get a queue) 0) (f paxType queue pax))))
        {}
        splits))
    paxLoads))

(defn workload
  [paxLoads]
  (wlplimpl paxLoads #(* (paxProcTime %1 %2) %3)))


(defn paxload
  [paxLoads]
  (wlplimpl paxLoads (fn [pt q p] p)))

(paxload myPaxLoads)

(workload myPaxLoads)

(defn -main [] (
                 print (paxload myPaxLoads)
                 (f/show-formatters)
                 ))
