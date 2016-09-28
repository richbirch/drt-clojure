(ns drt-clj.core
  (:gen-class))

(def flight {:code "BA0001" :pax 90})

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

(def procTimes
  {
    "emr" {"eea-desk" 0.25 "e-gate" 0.2}
    "enmr" {"eea-desk" 0.75}
    "visa" {"non-eea-desk" 0.5 "fast-track" 0.25}
    "non-visa" {"non-eea-desk" 0.8 "fast-track" 0.3}
    }
  )

(get (get procTimes "emr") "e-gate")

(defn paxProcTime
  [paxType queue]
  (get (get procTimes paxType) queue)
  )

(defn splitPax
  [pax-queue-splits pax-mins]
  (map (fn [pax]
         (into []
               (map (fn [split]
                      {:paxType (:paxType split) :queue (:queue split) :pax (* (:ratio split) pax)})
                    pax-queue-splits)))
       pax-mins))


(into (repeat 3 20) [5])

(paxByMinute 90 25)

(def myPaxLoads (splitPax splitRatios (paxByMinute (flight :pax) paxDisRate)))

myPaxLoads

(defn workload
  [paxLoads]
  (map
    (fn [splits]
      (reduce
        (fn [a {paxType :paxType queue :queue pax :pax}]
          (assoc a queue (+ (or (get a queue) 0) (* (paxProcTime paxType queue) pax)))
          )
        {}
        splits
        )
      )
    myPaxLoads
    )
  )

(defn paxload
  [paxLoads]
  (map
    (fn [splits]
      (reduce
        (fn [a {paxType :paxType queue :queue pax :pax}]
          (assoc a queue (+ (or (get a queue) 0) pax))
          )
        {}
        splits
        )
      )
    myPaxLoads
    )
  )

(paxload paxLoads)

(workload paxLoads)
