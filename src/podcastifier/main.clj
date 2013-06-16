(ns podcastifier.main
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [dynne.sound :as dynne :refer :all]))

;;; File management

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

;;; Time

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

(defn add-time
  "Sums `times`"
  [& times]
  (reduce + (map normalize-time times)))

(defn subtract-time
  "Subtracts time `t2` from `t1`"
  [t1 t2]
  (- (normalize-time t1) (normalize-time t2)))

;;; External process integration

(defn sh
  "Invokes the specified command with `args`"
  [command & args]
  (let [{:keys [exit out err]} (apply sh/sh command (map str args))]
    (when-not (zero? exit)
      (throw (ex-info (str "Invocation of " command " failed")
                      {:reason :command-invocation-failure
                       :args args
                       :command command
                       :exit exit
                       :out out
                       :err err})))))
(defn sox
  "Returns an invocation of sox with the specified arguments"
  [& args]
  (apply sh "sox" args))


;;; Podcast production

(defn read-config
  "Reads the podcast configuration file at `path` and returns the
  corresponding data structure."
  [path]
  (edn/read
   {:readers (merge default-data-readers
                    {'duration normalize-time})}
   (-> path io/reader (java.io.PushbackReader.) )))

(defn peak
  "Returns the maximum absolute amplitude of `s` on channel `c` when
  sampled at `sample-rate`. If provided, will return immediately on
  finding a value above `limit`."
  ([s sample-rate] (peak s sample-rate Double/MAX_VALUE))
  ([s sample-rate limit]
     (let [max-t (duration s)
           inc-t (/ 1.0 sample-rate)
           max-c (- (channels s) 1)]
       (loop [t 0.0
              c 0
              max-amplitude Double/MIN_VALUE]
         (if (< t max-t)
           (let [s (sample s t c)
                 new-max (max max-amplitude (Math/abs s))]
             (if (<= limit new-max)
               new-max
               (if (< c max-c)
                 (recur t (+ c 1) new-max)
                 (recur (+ t inc-t) 0 new-max))))
           max-amplitude)))))

(defn voices
  "Returns a Sound for the voices part of the podcast given `voices-config`."
  [voices-config]
  (let [{:keys [start end pan?]
         fade-duration :fade-in} voices-config
         v (-> voices-config :both read-sound)
         v (trim v (subtract-time start fade-duration) end)
         v (if pan? (pan v 0.4) v)
         v-max (peak v 4000 0.99)]
    (-> v
        (gain (/ 1.0 v-max))
        (fade-in fade-duration))))

(defn intro-music
  "Returns a Sound for the intro-music part of the podcast given
  `intro-config` and `voices-config`."
  [voices-config intro-config]
  (let [fade-out-duration (subtract-time (:intro-music-fade voices-config)
                                         (:start voices-config))
        intro-fade        (segmented-linear
                           1.0                         (:full-volume-length intro-config)
                           (:fade-amount intro-config) (:fade-in voices-config)
                           (:fade-amount intro-config) fade-out-duration
                           0)]
    (-> intro-config
        :file
        read-sound
        (multiply intro-fade))))

(defn outro-music
  "Returns a sound for the outro-music part of the podcast given
  `outro-config` and `voices-config`"
  [voices-config outro-config]
  (let [
        ;; outro-fade (segmented-linear
        ;;             (:fade-amount outro-config) (- (:end voices-config)
        ;;                                            (:outro-music-start voices-config))
        ;;             (:fade-amount outro-config) (:fade-up outro-config)
        ;;             1.0 
        ;;             1.0                        (:full-volume-length outro-config)
        ;;            .0)
        ]
   (-> outro-config
       :file
       read-sound
       )))

(defn normalize
  "Returns a version of s scaled so that the peak absoute amplitude is
  near 1.0"
  [s]
  (let [p (peak s 4000 0.99)]
    (gain s (/ 1.0 p))))

(defn bumper
  "Returns a sound for the bumper part of the podcast, containing the
  voice and music mixed togheter."
  [path music-config]
  (let [b (read-sound path)]
    (-> music-config
        :file
        read-sound
        (gain (:fade-amount music-config))
        (trim (:start-at music-config) Double/MAX_VALUE)
        (trim 0 (+ (duration b) (:fade-out music-config)))
        (fade-out (:fade-out music-config))
        (mix b)
        normalize)))


(defn -main
  "Entry point for the application"
  [config-path]
  (let [config        (read-config config-path)
        voices-config (:voices config)
        intro-config  (-> config :music :intro)
        outro-config  (-> config :music :outro)
        v             (voices voices-config)
        i             (intro-music voices-config intro-config)
        v+i           (-> v
                          (timeshift (-> intro-config :full-volume-length))
                          (mix i))
        bumper        (bumper (:bumper config) (-> config :music "bumper"))
        bumper-bloop  (-> config :bloops :bumper read-sound)
        end-bloop     (-> config :bloops :end read-sound)
        o-music       (outro-music voices-config outro-config)



        ;; outro-fade-up-start  (subtract-time (-> config :voices :end)
        ;;                                     (-> config :voices :outro-music-start))
        ;; outro-fade-up-end    (add-time outro-fade-up-start
        ;;                                (-> config :music :outro :fade-up))
        ;; outro-fade-out-start (add-time outro-fade-up-end
        ;;                                (-> config :music :outro :full-volume-length))
        ;; outro-fade-out-end   (add-time outro-fade-out-start
        ;;                                (-> config :music :outro :fade-out))
        ;; outro                (-> config :music :outro :file
        ;;                          (to-wav voice-rate)
        ;;                          (fade
        ;;                           [[(-> config :music :outro :fade-amount) 0]
        ;;                            [(-> config :music :outro :fade-amount) outro-fade-up-start]
        ;;                            [1.0 outro-fade-up-end]
        ;;                            [1.0 outro-fade-out-start]
        ;;                            [0.0 outro-fade-out-end]])
        ;;                          (match-sample-rate voice)
        ;;                          (trim 0.0 outro-fade-out-end))
        final         nil
        ]
    (save final "episode.wav" 44100)

    )

  #_(let [config ]
      (let [
            voice (-> config :voices :both
                      pan-f
                      (trim
                       (subtract-time
                        (-> config :voices :start)
                        (-> config :voices :fade-in))
                       (-> config :voices :end))
                      (fade-in
                       (-> config :voices :fade-in)))
            voice-rate (rate voice)
            intro-soft-start (add-time (-> config :music :intro :full-volume-length)
                                       (-> config :voices :fade-in))
            intro-soft-end (add-time intro-soft-start
                                     (subtract-time (-> config :voices :intro-music-fade)
                                                    (-> config :voices :start)))
            intro-end (add-time intro-soft-end (-> config :music :intro :fade-out))
            intro (-> config :music :intro :file
                      (to-wav voice-rate)
                      (fade
                       [[1.0 (-> config :music :intro :full-volume-length)]
                        [(-> config :music :intro :fade-amount) intro-soft-start]
                        [(-> config :music :intro :fade-amount) intro-soft-end]
                        [0.0 intro-end]])
                      (trim 0.0 intro-end))
            outro-fade-up-start (subtract-time (-> config :voices :end)
                                               (-> config :voices :outro-music-start))
            outro-fade-up-end (add-time outro-fade-up-start
                                        (-> config :music :outro :fade-up))
            outro-fade-out-start (add-time outro-fade-up-end
                                           (-> config :music :outro :full-volume-length))
            outro-fade-out-end (add-time outro-fade-out-start
                                         (-> config :music :outro :fade-out))
            outro (-> config :music :outro :file
                      (to-wav voice-rate)
                      (fade
                       [[(-> config :music :outro :fade-amount) 0]
                        [(-> config :music :outro :fade-amount) outro-fade-up-start]
                        [1.0 outro-fade-up-end]
                        [1.0 outro-fade-out-start]
                        [0.0 outro-fade-out-end]])
                      (match-sample-rate voice)
                      (trim 0.0 outro-fade-out-end))
            outro-music-start (-> (-> config :voices :outro-music-start)
                                  (subtract-time (-> config :voices :start))
                                  (add-time (-> config :voices :fade-in)))
            voice-with-outro (mix voice outro outro-music-start)
            voice-with-intro (mix intro voice-with-outro
                                  (-> config :music :intro :full-volume-length))
            bumper-length (length (-> config :bumper))
            bumper-music-start (-> config :music :bumper :start-at)
            bumper-music-fade-start (add-time bumper-music-start bumper-length)
            bumper-music-end (add-time bumper-music-fade-start
                                       (-> config :music :bumper :fade-out))
            bumper-fade (-> config :music :bumper :fade-amount)
            bumper-music (-> config :music :bumper :file
                             (to-wav voice-rate)
                             (trim bumper-music-start bumper-music-end)
                             (fade [[bumper-fade 0.0]
                                    [bumper-fade bumper-length]
                                    [0.0 (add-time bumper-length
                                                   (-> config :music :bumper :fade-out))]]))
            bumper-with-music (-> config :bumper
                                  (to-wav voice-rate)
                                  (mix bumper-music 0.0))
            final (append bumper-with-music
                          (silence voice 1)
                          (to-wav (-> config :bloops :bumper) voice-rate)
                          (silence voice 3)
                          voice-with-intro
                          (silence voice 2)
                          (to-wav (-> config :bloops :end) voice-rate))]
        {:bumper-with-music bumper-with-music
         :voice-with-intro voice-with-intro
         :final final})))
