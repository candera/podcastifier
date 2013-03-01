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

(defn pan
  [config input]
  (if (-> config :voices :pan?)
    (let [output (new-file)]
      {:commands [(sox input output "remix" "-p" "1,2v0.6" "1v0.6,2")]
       :output   output})
    {:output input}))

(defn trim-voices
  [config input]
  (let [output (new-file)
        trim1 ()]
   {:commands [(sox input output )]
    :output output}))

(defn fade-in-voices
  [config input]
  {:commands ["# TODO: fade in"]
   :output input})

(defn apply-step
  [config state step]
  (let [{:keys [commands output]} (step config (:output state))]
    (-> state
        (update-in [:commands] into commands)
        (assoc :output output))))

(defn commands
  [config]
  (:commands (reduce #(apply-step config %1 %2) 
                     {:commands []} 
                     [pan 
                      trim-voices
                      fade-in-voices])))

(defn -main
  "Entry point for the application"
  [config-path]
  (let [config (-> config-path io/reader (java.io.PushbackReader.) edn/read)]
    (print-header)
    (doseq [c (commands config)]
      (when c (println c)))))
