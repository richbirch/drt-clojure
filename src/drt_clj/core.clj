(ns drt-clj.core
  (:gen-class))

(require '[clj-time.core :as t])
(require '[clj-time.format :as f])
(require '[clj-time.coerce :as c])

(def flight {:code "BA0001" :scheduled "2016-10-31T07:20:05.000Z" :pax 90})

(def paxDisRate 25)


(def splitRatios [
                   {:paxType :eea-mr :queue :eea-desk :ratio 0.6}
                   {:paxType :eea-mr :queue :e-gate :ratio 0.2}
                   {:paxType :eea-nmr :queue :eea-desk :ratio 0.2}
                   ])

(defn queueTypesFromSplitRatios
  [sr]
  (into #{} (map #(:queue %) sr)))

(def procTimes
  {
    :eea-mr {:eea-desk 0.25 :e-gate 0.2}
    :eea-nmr {:eea-desk 0.75}
    :visa {:non-eea-desk 0.5 :fast-track 0.25}
    :non-visa {:non-eea-desk 0.8 :fast-track 0.3}})

;;;

(defn pcpMinutes
  [pax rate scheduled]

    (def start (dateStrToLong scheduled))
    (def nbMins (int (+ (Math/ceil (/ pax rate)))))
    (range start (+ start (* nbMins 60000)) 60000)
  )

(defn paxMinutes
  [pax rate]
  (reverse (conj (repeat (int (/ pax rate)) rate) (mod pax rate))))

(defn paxByMinute
  [pax rate scheduled]
  (zipmap (pcpMinutes pax rate scheduled) (paxMinutes pax rate)))

;;;

(paxByMinute (flight :pax) paxDisRate (flight :scheduled))

(defn dateStrToLong
  [scheduled]
  (c/to-long (f/parse scheduled)))

(defn paxProcTime
  [paxType queue]
  (get (get procTimes paxType) queue))

(defn splitPax
  [pax-queue-splits pax-mins]
  (map (fn [minute-pax]
         {:minute (first minute-pax)
          :load (map (fn [split]
                       {:paxType (:paxType split) :queue (:queue split) :pax (* (:ratio split) (second minute-pax))})
                    pax-queue-splits)})
       pax-mins))

(def pax-type-queue-loads (splitPax splitRatios (paxByMinute (flight :pax) paxDisRate (flight :scheduled))))

pax-type-queue-loads


(defn wlplimpl
  [paxLoads f]
  (into {}
    (map
      (fn [{minute :minute splits :load}]
        [(keyword (str minute))
         (reduce
          (fn [a {paxType :paxType queue :queue pax :pax}]
            (assoc a queue (+ (or (get a queue) 0) (f paxType queue pax))))
          {}
          splits)])
      paxLoads)))

(defn workload
  [paxLoads]
  (wlplimpl paxLoads #(* (paxProcTime %1 %2) %3)))

(defn paxload
  [paxLoads]
  (wlplimpl paxLoads (fn [pt q p] p)))

(def queue-paxload (paxload pax-type-queue-loads))
(def queue-workload (workload pax-type-queue-loads))

(map (fn [{minute :minute}] (c/from-long minute)) (paxload pax-type-queue-loads))

(defn firstMinute
  [minute-loads]
  (apply min (map (fn [{minute :minute}] minute) minute-loads)))

(defn minutesOfHours [hours start] (range start (+ start (* 60000 60 hours)) 60000))

(minutesOfHours 24 (firstMinute pax-type-queue-loads))

(def emptyQueues
  (reduce
    (fn
      [agg qt]
      (assoc agg qt 0))
    {}
    (queueTypesFromSplitRatios splitRatios)))

emptyQueues
pax-type-queue-loads


(reduce
  (fn
    [agg m]
    (def min-key (keyword (str m)))
    (assoc agg min-key (min-key queue-paxloads emptyQueues)))
  {}
  (minutesOfHours 24 (firstMinute pax-type-queue-loads)))

(defn fillDay
  [pax-type-queue-loads]
  (map (fn [m] (or (and (:minute m) (  :loads {:eea-desk (get pax-type-queue-loads :minute)}))))))

;;

(defn -main [] (
                 print (paxload pax-type-queue-loads)
                 (f/show-formatters)
                 ))
