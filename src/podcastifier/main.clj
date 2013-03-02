(ns podcastifier.main
  (:gen-class)
  (:refer-clojure :exclude [println])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn println
  "Prints without a stupid carriage return, which is screwing me."
  [& args]
  (apply print args)
  (print \newline))

(defn print-header
  "Print stuff that should go at the top of the script"
  []
  (println "#!/bin/bash")
  (println))

(def file-number (atom 0))

(defn tempfile-name
  [n]
  (format "tempfile-%06d.wav" n))

(defn new-file
  []
  (tempfile-name (swap! file-number inc)))

(defn last-file
  []
  (tempfile-name @file-number))

(defn normalize-time
  "Returns an [h m s] tuple given a time-like object, which is either a
  tuple or string like \"02:15:22\""
  [t]
  (cond
   (vector? t)
   (let [[h m s] t
         s1 (mod s 60)
         m1 (+ m (int (Math/floor (/ s 60))))
         m2 (mod m1 60)
         h1 (+ h (int (Math/floor (/ m1 60))))]
     [h1 m2 s1])
   
   (string? t)
   (->> (str/split t #":")
        (into [])
        (mapv #(Long/parseLong %))
        (normalize-time))
   
   :else
   (throw (ex-info "Unsupported timelike" 
                   {:reason :unsupported-timelike
                    :t t}))))

(defn time->str
  "Return a string like \"01:02:03\" given a timelike value."
  [t]
  (let [[h m s] (normalize-time t)]
    (format "%02d:%02d:%02d" h m s)))

(defn subtract-time
  "Subtracts time t2 from t1"
  [t1 t2]
  (let [t1* (normalize-time t1)
        t2* (normalize-time t2)]
    (normalize-time (mapv - t1* t2*))))

(defn sox
  "Returns an invocation of sox with the specified arguments"
  [& args]
  (->> args
       (concat ["sox"])
       (interpose " ")
       (apply str)))

;; Processing steps

(defn no-op
  [input]
  {:commands []
   :output input})

(defn pan
  [input]
  (let [output (new-file)]
    {:commands [(sox input output "remix" "-p" "1,2v0.6" "1v0.6,2")]
     :output   output}))

(defn trim-voices
  [input start end fade-up]
  (let [output (new-file)
        trim1 (-> start normalize-time (subtract-time fade-up) time->str)
        trim2 (-> end time->str)]
    {:commands [(sox input output "trim" (str "=" trim1) (str "=" trim2))]
     :output output}))

(defn fade-in
  [input duration]
  (let [output (new-file)]
   {:commands [(sox input output "fade" "l" duration)]
    :output output}))

(defn fade-down
  "Fades the input from full volume down to `amount` over `duration`
  beginning at `start`." 
  [input start duration amount]
  (let [output (new-file)]
    (throw (ex-info "Not yet implemented" {:reason :not-yet-implemented}))))

(defn fade-out
  "Fades the input to zero for the last `duration`."
  [input duration]
  (throw (ex-info "Not yet implemented" {:reason :not-yet-implemented})))

(defn apply-step
  [state step]
  (let [{:keys [commands output]} (step (:output state))]
    (-> state
        (update-in [:commands] into commands)
        (assoc :output output))))

(defn apply-chain
  [input steps]
  (reduce apply-step {:commands [] :output input} steps)) 

(defn commands
  [config]
  (let [voice (apply-chain (-> config :voices :both)
                           [(if (-> config :voices :pan?) pan no-op)
                            #(trim-voices % 
                                          (-> config :voices :start)
                                          (-> config :voices :end)
                                          (-> config :voices :fade-in))
                            #(fade-in %
                                      (-> config :voices :fade-in))])
        intro (apply-chain (-> config :music :intro)
                           [#(fade-down %
                                        (-> config :music :intro :full-volume-length)
                                        (-> config :voices :fade-in)
                                        (-> config :music :intro :fade-amount))
                            #(fade-out %
                                       (-> config :music :intro :fade-out))])]
    
    (:commands voice)))

(defn -main
  "Entry point for the application"
  [config-path]
  (let [config (-> config-path io/reader (java.io.PushbackReader.) edn/read)]
    (print-header)
    (doseq [c (commands config)]
      (when c (println c)))))
