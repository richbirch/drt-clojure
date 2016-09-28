(ns drt-clj.core
  (:gen-class))

(def flight {:code "BA0001" :pax 100})

(def paxDisRate 25)

(defn paxByMinute
  [pax, disRate]
  (if (>= pax disRate)
    (into [disRate] (paxByMinute (- pax disRate) disRate))
    (if (> 0 pax)
      (into [] [pax]))))

(def splitRatios [
                  {:paxType "emr" :queue "eea-desk" :ratio 0.6}
                  {:paxType "emr" :queue "e-gate" :ratio 0.2}
                  {:paxType "enmr" :queue "eea-desk" :ratio 0.2}
                  ])

(defn splitPax
  [pax-queue-splits pax-mins]
  (map (fn [pax]
         (into []
               (map (fn [split]
                      {:paxType (:paxType split) :queue (:queue split) :ratio (* (:ratio split) pax)})
                    pax-queue-splits)))
       pax-mins))



(def paxLoads (splitPax splitRatios (paxByMinute (flight :pax) paxDisRate)))

paxLoads

(reduce (fn [queue-paxloads {queue :queue pax :pax}]
          (assoc queue-paxloads queue (+ (or (queue-paxloads :queue) 0) pax)))
        {}
        paxLoads)

(reduce (fn [queue-paxloads thing]
          (println thing))
        {}
        paxLoads)

(+ 5 (or ({:ye 1} :yeah) 0))

