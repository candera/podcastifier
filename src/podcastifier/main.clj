(ns podcastifier.main
  (:gen-class)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [dynne.sampled-sound :as dynne :refer :all]))

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

(def ^:dynamic *file-base* nil)

(defn parent
  "Given a path, return its parent."
  [path]
  (.getParent (io/file path)))

(defn relative-path
  "Given a path, return it relative to `base`."
  [path base]
  (let [f (io/file path)]
    (if (.isAbsolute f)
      path
      (if base
        (-> (io/file base path) .getCanonicalPath)
        path))))

;;; Volume

(defn read-decibel
  "Returns a linear-scale floating point amplitude differential given an amplitude differential expressed in decibels. For instance, -10.0 returns "
  [db]
  (Math/pow 10.0 (/ db 10.0)))

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
                    {'duration normalize-time
                     'db read-decibel})}
   (-> path io/reader (java.io.PushbackReader.) )))

(defn voices
  "Returns a Sound for the voices part of the podcast given `voices-config`."
  [base-dir voices-config footer]
  (let [{:keys [start end pan?]
         fade-duration :fade-in} voices-config
         v (-> voices-config :both (relative-path base-dir) read-sound)
         v (trim v (subtract-time start fade-duration) end)
         v (if pan? (pan v 0.4) v)
         v-max (peak v 4000 0.99)]
    (-> v
        (gain (/ 1.0 v-max))
        (fade-in fade-duration)
        (append (silence (+ (* 2 (:footer-fade-up-down voices-config))
                            (:footer-padding voices-config))
                         2))
        (append (->stereo footer)))))

(defn intro-envelope
  "Returns a Sound that provides a fade envelope for the intro music."
  [voices-config intro-config]
  (let [quiet-duration (subtract-time (:intro-music-fade voices-config)
                                      (:start voices-config))]
    (segmented-linear
     2
     (get intro-config :full-volume-level 1.0)  (:full-volume-length intro-config)
     (get intro-config :full-volume-level 1.0)  (:fade-in voices-config)
     (:fade-amount intro-config) quiet-duration
     (:fade-amount intro-config) (:fade-out intro-config)
     0)))

(defn intro-music
  "Returns a Sound for the intro-music part of the podcast given
  `intro-config` and `voices-config`."
  [base-dir voices-config intro-config]
  (-> intro-config
      :file
      (relative-path base-dir)
      read-sound
      (envelope (intro-envelope voices-config intro-config))))

(defn outro-music
  "Returns a sound for the outro-music part of the podcast given
  `outro-config` and `voices-config`"
  [base-dir voices-config outro-config footer]
  (println "outtro" (get outro-config :full-volume-level 1.0))
  (let [outro-fade (segmented-linear
                    2
                    (:fade-amount outro-config) (- (:end voices-config)
                                                   (:outro-music-start voices-config))
                    (:fade-amount outro-config)               (:footer-fade-up-down voices-config)
                    (get outro-config :full-volume-level 1.0) (:footer-padding voices-config)
                    (get outro-config :full-volume-level 1.0) (:footer-fade-up-down voices-config)
                    (:fade-amount outro-config)               (duration footer)
                    (:fade-amount outro-config)               (:fade-up outro-config)
                    (get outro-config :full-volume-level 1.0) (:full-volume-length outro-config)
                    (get outro-config :full-volume-level 1.0) (:fade-out outro-config)
                    0.0) ]
    (-> outro-config
        :file
        (relative-path base-dir)
        read-sound
        (envelope outro-fade))))

(defn normalize
  "Returns a version of s scaled so that the peak absoute amplitude is
  near 1.0"
  [s]
  (let [p (peak s 16000 0.99)]
    (gain s (/ 1.0 p))))

(defn bumper
  "Returns a sound for the bumper part of the podcast, containing the
  voice and music mixed togheter."
  [base-dir path music-config]
  (let [b (read-sound (relative-path path base-dir))]
    (-> music-config
        :file
        (relative-path base-dir)
        read-sound
        (gain (:fade-amount music-config))
        (trim (:start-at music-config) Double/MAX_VALUE)
        (trim 0 (+ (duration b) (:fade-out music-config)))
        (fade-out (:fade-out music-config))
        (mix (->stereo b))
        normalize
        )))

(defn episode
  [config-path]
  (let [config        (read-config config-path)
        base-dir      (.getParent (io/file config-path))
        voices-config (:voices config)
        intro-config  (-> config :music :intro)
        outro-config  (-> config :music :outro)
        footer        (-> config :footer (relative-path base-dir) read-sound)
        v             (voices base-dir voices-config footer)
        i             (intro-music base-dir voices-config intro-config)
        o             (outro-music base-dir voices-config outro-config footer)
        ivo           (-> v
                          (mix (timeshift o (+ (:fade-in voices-config)
                                               (- (:outro-music-start voices-config)
                                                  (:start voices-config)))))
                          (timeshift (:full-volume-length intro-config))
                          (mix i))
        bumper        (bumper base-dir (:bumper config) (-> config :music :bumper))
        bumper-bloop  (-> config :bloops :bumper (relative-path base-dir) read-sound)
        end-bloop     (-> config :bloops :end (relative-path base-dir) read-sound)
        final         (-> bumper
                          (append (silence 1.0 2))
                          (append (->stereo bumper-bloop))
                          (append (silence 3.0 2))
                          (append ivo)
                          (append (silence 2.0 2))
                          (append (->stereo end-bloop)))]
    {:base-dir       base-dir
     :config         config
     :v              v
     :i              i
     :o              o
     :ivo            ivo
     :bumper         bumper
     :final          final}))

(defn episode-file-name
  [config]
  (format "%s-%03d-%s.wav" (:show-name config) (:number config) (:label config)))

(defn -main
  "Entry point for the application"
  [config-path]
  (let [ep (episode config-path)]
    (save (:final ep)
          (relative-path (episode-file-name (:config ep)) (:base-dir ep))
          16000)))
