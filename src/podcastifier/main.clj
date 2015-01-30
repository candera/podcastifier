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


(defn normalize
  "Returns a version of s scaled so that the peak absoute amplitude is
  near 1.0"
  [s]
  (let [p (peak s 16000 0.99)]
    (gain s (/ 1.0 p))))


(defn has-keys?
  "Return true if all of the keys are defined in the map"
  [m & keys]
  (every? (partial contains? m) keys))


(defn trim-sound
  "Given a sound config and a sound, trim the sound according to the config.
  The specific trimming done depends on what is in the config:
  * If :start and :end are specified, that's what gets used.
  * If :start and :duration are specified, you get that.
  * If :end and :duration is used, you get a sound that ends on end and is duration long.
  * If only :start is specified, you get :start->end of the sound.
  * If only :end is specified, you get 0->:end.
  * If only :duration is specified, you get 0->:duration.
  * If :tail is specified, you get a sound that is :tail long, with the front clipped off.
  * None of the above gives you the original sound."
  [s config]
  (let [[start end]
        (cond
         (has-keys? config :start :end) [(:start config) (:end config)]
         (has-keys? config :start :duration) [(:start config) (+ (:start config) (:duration config))]
         (has-keys? config :end :duration) [(- (:end config) (:duration config)) (:end config)]
         (has-keys? config :start) [(:start config) (duration s)]
         (has-keys? config :end) [0.0 (:end config)]
         (has-keys? config :duration) [0.0 (:duration config)]
         (has-keys? config :tail) [(- (duration s) (:tail config)) (duration s)]
         :default [0.0 (duration s)])
        start (if (:start-pad config) (- start (:start-pad config)) start)
        end (if (:end-pad config) (+ end (:end-pad config)) end)
        start (Math/max 0.0 (double start))
        end (Math/min (duration s) (double end))]
    (trim s start end)))

(defn fade-sound
  [s config]
  (let [s (if (:fade-in config) (fade-in s (:fade-in config)) s)
        s (if (:fade-out config) (fade-out s (:fade-out config)) s)]
    s))

(defn read-sound-file
  "Read a sound file according to the instructions in config.
  The sound can be trimmed, amplified, normalized, doubled into stereo,
  panned, faded or have silence added to the front or back if the
  config specifies."
  [base-dir config]
  (let [sound (-> config :file (relative-path base-dir) read-sound)
        sound (trim-sound sound config)
        sound (if (:gain config) (gain sound (:gain config)) sound)
        sound (if (:normalize config) (normalize sound) sound)
        sound (if (:->stereo config) (->stereo sound) sound)
        sound (if (:pan config) (pan sound (:pan config)) sound)
        sound (fade-sound sound config)
        sound (if (:pre-silence config) (append (silence (:pre-silence config) 2) sound) sound)
        sound (if (:post-silence config) (append sound (silence (:post-silence config) 2)) sound)]
    sound))


(defn two-step-fade-in
  "Fade the sound such that it starts from 0 and fades up to fade-level,
  stays there for about overlap seconds and then fades up to 1."
  [s overlap fade-t fade-level]
  (let [quiet-t (- overlap fade-t)]
    (envelope
     (segmented-linear
      2
      0.0        fade-t
      fade-level quiet-t
      fade-level fade-t
      1.0        (duration s)
      1.0)
     s)))


(defn append-music
  "Append the music to the voices with appropriate fades.
  The result has the music fading in in two steps
  as with voices end. The two sounds overlap
  for duration overlap."
  [voices music overlap fade-t fade-level]
  (let [join-t (- (duration voices) overlap)
        faded-music (two-step-fade-in music overlap fade-t fade-level)]
    (mix voices (timeshift faded-music join-t))))


(defn two-step-fade-out
  "Fade the sound such that it drops to fade-level overlap seconds
  from the end and then fades out completely."
  [s overlap fade-t fade-level]
  (let [start-t (-  (duration s)overlap)
        quiet-t (- overlap (* 2 fade-t))]
    (envelope
     (segmented-linear
      2
      1.0      start-t
      1.0      fade-t
      fade-level quiet-t
      fade-level fade-t
      0.0        (duration s)
      0.0)
     s)))

(defn prepend-music
  "Join music and voices together, so that they overlap by
  overlap seconds. The music fades down to fade-level during
  the overlap."
  [music voices overlap fade-t fade-level]
  (let [faded-music (two-step-fade-out music overlap fade-t fade-level)]
    (mix faded-music (timeshift voices (- (duration music) overlap)))))


(defn duck-fade
  "Fade s down to fade-db between t1 and t2"
  [s t1 t2 fade-t duck-level]
  (let [quiet-t (- t2 t1)]
    (envelope
     (segmented-linear
      2
      1.0
      t1  1.0
      fade-t   duck-level
      quiet-t  duck-level
      fade-t   1.0
      (duration s) 1.0)
     s)))


(defn duck
  "Duck s2 into s1 starting at t.
  Essentially we mix s2 into s1 at time t, fading s1 down to level duck-level
  while keeping s1 at full volume."
  [s1 s2 t fade-t duck-level]
  (let [overlap (* fade-t 0.8)
        t1 (- t overlap)
        t2 (- (+ t1 (duration s2)) overlap)
        s1 (duck-fade s1 t1 t2 fade-t duck-level)]
    (mix s1 (timeshift s2 t))))



(defn bumper
  "Returns a sound for the bumper part of the podcast, containing the
  bumper voice and music mixed togheter."
  [base-dir bumper-config music-config]
  (let [extra (get music-config :extra 4.0)
        b (read-sound-file base-dir bumper-config)
        m (read-sound-file base-dir music-config)
        m (trim m 0 (+ extra (duration b)))
        m (fade-out m extra)]
     (mix (->stereo(normalize b)) m)))


(defn episode
  [config-path]
  (let [config        (read-config config-path)
        base-dir      (.getParent (io/file config-path))
        voices-config (:voices config)
        intro-config  (-> config :music :intro)
        outro-config  (-> config :music :outro)
        bumper-config (-> config :music :bumper)
        footer-config (-> config :footer)
        easter-config (-> config :easter-egg)
        start-bloop   (->> config :start-bloop (read-sound-file base-dir))
        end-bloop     (->> config :end-bloop (read-sound-file base-dir))
        footer        (read-sound-file base-dir footer-config)
        mixed-bumper  (bumper base-dir (:bumper config) (-> config :music :bumper))
        i             (read-sound-file base-dir intro-config)
        v             (read-sound-file base-dir voices-config)
        o             (read-sound-file base-dir outro-config)
        of            (duck o footer (:offset footer-config)
                                     (:overlap-fade-updown footer-config)
                                     (:overlap-fade footer-config))
        iv            (prepend-music i
                                     v
                                     (:overlap intro-config)
                                     (:overlap-fade-updown intro-config)
                                     (:overlap-fade intro-config))
        ivof          (append-music iv
                                    of
                                    (:overlap outro-config)
                                    (:overlap-fade-updown outro-config)
                                    (:overlap-fade outro-config))
        show          (-> mixed-bumper
                          (append start-bloop)
                          (append ivof)
                          (append end-bloop))
        easter-egg    (read-sound-file base-dir easter-config)
        final         (if easter-egg (append show easter-egg) show)]
    {:base-dir       base-dir
     :config         config
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




