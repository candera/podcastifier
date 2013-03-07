(ns podcastifier.main
  (:gen-class)
  (:refer-clojure :exclude [println])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [WavFile WavFile]))

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
  "Returns a normalized time value given a time-like object, which is
  either a [h m s] tuple, a floating point number of seconds, or a
  string like \"02:15:22\""
  [t]
  (cond
   (vector? t)
   (let [[h m s] t

         s1 (mod s 60)
         m1 (+ m (int (Math/floor (/ s 60))))
         m2 (mod m1 60)
         h1 (+ h (int (Math/floor (/ m1 60))))]
     (+ (* 3600 h) (* 60 m) s))

   (number? t) t

   (string? t)
   (let [[h m s] (str/split t #":")]
     (normalize-time [(Long/parseLong h)
                      (Long/parseLong m)
                      (Double/parseDouble s)]))

   :else
   (throw (ex-info "Unsupported timelike"
                   {:reason :unsupported-timelike
                    :t t}))))

(defn time->str
  "Return a string like \"01:02:03\" given a timelike value."
  [t]
  (let [s (normalize-time t)
        h (int (/ s 3600))
        s* (- s (* h 3600))
        m (int (/ s* 60))
        s** (- s* (* m 60))]
    (format "%02d:%02d:%s%2.3f" h m (if (< s** 10) "0" "") s**)))

(defn fade-value
  "Returns the amount to fade by given a time and a `controls` spec
  like the one given to `fade`, except where the time values must have
  been normalized first."
  [t controls]
  (let [[last-v last-t] (last controls)]
    (if (<= last-t t)
      last-v
      (let
          [c* (apply vector [1.0 0] controls)
           control-pairs (partition 2 1 c*)
           [start-v start-t end-v end-t]
           (some (fn [[[start-v start-t] [end-v end-t]]]
                   (when (<= start-t t end-t)
                     [start-v start-t end-v end-t]))
                 control-pairs)]
        (if (= end-t start-t)
          start-v
          (let [delta-v (- (double end-v) start-v)
                delta-t (- (double end-t) start-t)
                v       (+ start-v
                           (* delta-v (/ (- (double t) start-t) delta-t)))]
            (* v v)))))))

(defn fade
  "Writes audio file to path `output` by adjusting the gain in `input`
  smoothly between the [gain time] points in `controls`, where gain is
  between 0.0 and 1.0 (inclusive) and time is as described below.
  There is an implicit control point at the end of the audio with the
  last gain specified.

  Times can be specified as a number, interpreted as seconds, as a
  vector of numbers, interpreted as [hours minutes seconds], or as a
  string, which must be of the form \"hh:mm:ss.sss\".

  Example: (fade \"foo.wav\" \"bar.wav\" [[0 0] [1 14.5]]) would fade
  the audio in smoothly until 14.5 seconds in, then stay at full
  volume for the rest of the file.

  Example: (fade \"foo.wav\" \"bar.wav\" [[1.0 0]
                                          [0.2 12.0]
                                          [0.2 \"00:01:30\"]
                                          [0 \"00:01:45\"]])

  would start at full volume, fade down to 20% volume at 12 seconds,
  stay there until 90 seconds in, and then fade out completely by 15
  seconds later."
  [input output controls]
  (let [input-file (io/file input)
        input-wav (WavFile/openWavFile input-file)
        channels (.getNumChannels input-wav)
        sample-rate (.getSampleRate input-wav)
        buffer-size 100
        input-buffer (double-array (* channels buffer-size))
        output-buffer (double-array (* channels buffer-size))
        output-file (io/file output)
        output-wav (WavFile/newWavFile output-file
                                       channels
                                       (.getNumFrames input-wav)
                                       (.getValidBits input-wav)
                                       sample-rate)
        normalized-controls (mapv (fn [[v t]] [v (normalize-time t)]) controls)]
    (loop [frame-count 0]
      (let [frames-read (.readFrames input-wav input-buffer buffer-size)]
       (when (pos? frames-read)
         (dotimes [n frames-read]
           ;; There is a way better way to do this, which is to keep
           ;; track of where we are in the controls array and scale based
           ;; on that. I only realized that after writing the (very
           ;; complicated) logic in `fade-value`.
           (let [t           (/ (double (+ frame-count n)) sample-rate)
                 fade-amount (fade-value t normalized-controls)]
             (dotimes [i channels]
               (let [index (+ i (* n channels))]
                 (aset output-buffer index
                       (* 1.0 fade-amount (aget input-buffer index)))))))
         (.writeFrames output-wav output-buffer frames-read)
         (recur (+ frame-count frames-read)))))
    (.close input-wav)
    (.close output-wav)))

(defn add-time
  "Adds time `t1` to `t2`"
  [t1 t2]
  (+ (normalize-time t1) (normalize-time t2)))

(defn subtract-time
  "Subtracts time `t2` from `t1`"
  [t1 t2]
  (- (normalize-time t1) (normalize-time t2)))

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
  (let [beginning (new-file)
        middle (new-file)
        end (new-file)
        output (new-file)]
    {:commands [(sox input beginning "trim" 0 start)
                (sox input middle "trim" start duration)
                (sox input end "trim" (add-time start duration) "-00:00:00")
                (sox end end "gain" amount)
                (sox middle middle)]}))

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
