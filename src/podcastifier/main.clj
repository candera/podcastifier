(ns podcastifier.main
  (:gen-class)
  (:refer-clojure :exclude [resolve])
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.walk :as w]
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
  #_(println "path:" path "base:" base)
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

(defn process-sound
  [sound config]
  (let [sound (trim-sound sound config)
        sound (if (:gain config) (gain sound (:gain config)) sound)
        sound (if (:normalize config) (normalize sound) sound)
        sound (if (:->stereo config) (->stereo sound) sound)
        sound (if (:pan config) (pan sound (:pan config)) sound)
        sound (fade-sound sound config)
        sound (if (:pre-silence config) (append (silence (:pre-silence config) 2) sound) sound)
        sound (if (:post-silence config) (append sound (silence (:post-silence config) 2)) sound)]
    sound))

(defn read-sound-file
  [base-dir name]
  #_(println "read sound file:" base-dir name)
  (let [sound (-> name (relative-path base-dir) read-sound)]
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
  [voices music & {:keys [overlap fade updown] :or {overlap 20.0 fade 0.2 updown 4.0}}]
  (let [join-t (- (duration voices) overlap)
        faded-music (two-step-fade-in music overlap updown fade)]
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
  [music voices & {:keys [overlap fade updown] :or {overlap 20.0 fade 0.2 updown 4.0}}]
  (let [faded-music (two-step-fade-out music overlap updown fade)]
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
  [s1 s2 & {:keys [offset fade updown] :or {offset 20.0 fade 0.2 updown 4.0}}]
  (let [overlap (* updown 0.8)
        t1 (- offset overlap)
        t2 (- (+ t1 (duration s2)) overlap)
        s1 (duck-fade s1 t1 t2 updown fade)]
    (mix s1 (timeshift s2 offset))))


(defn background
  "Returns a sound for the bumper part of the podcast, containing the
  bumper voice and music mixed togheter."
  [fg bg & {:keys [extra] :or {extra 5.0}}]
  (let [bg (trim bg 0 (+ extra (duration fg)))
        music (fade-out bg extra)]
     (mix (->stereo(normalize fg)) bg)))

(defn episode-file-name
  [config]
  (format "%s-%03d-%s.wav" (:show-name config) (:number config) (:label config)))


(declare resolve)

(defn resolve-reduce [config f s]
  (println "Resulve reduce" config)
  (println "Res reduce" f)
  (println "Res reduce" s)
  (reduce f (map (partial resolve config) s)))


(defn append-all[config s]
  (resolve-reduce config append s))

(defn mix-all [config s]
  (println "mix:" s)
  (resolve-reduce config mix s))

(defn resolve-list [config l]
  (let [verb (first l)
        args (rest l)
        res (partial resolve config)
        a (first args)
        b (second args)]
    (cond
     (= verb 'background) (apply background  (res a) (res b) (drop 2 args))
     (= verb 'fade-out-music) (apply prepend-music (res a) (res b) (drop 2 args))
     (= verb 'fade-in-music) (apply append-music (res a) (res b) (drop 2 args))
     (= verb 'duck) (apply duck (res a) (res b) (drop 2 args))
     (= verb 'mix) (mix-all args)
     (= verb 'timeshift) (timeshift (res a) b)
     :default (throw
               (Exception.
                (str "Don't know how to resolve verb " verb " " (class verb) " from " l))))))

(defn resolve [config id]
  #_(println "resolve id" id)
  #_(println "resolve config:" config)
  (let [result
        (cond
         (string? id) (read-sound-file (:base-dir config) id)
         (keyword? id) (resolve config (id config))
         (map? id)  (process-sound (resolve config (:source id)) id)
         (vector? id) (append-all config id)
         (set? id) (mix-all config id)
         (list? id) (resolve-list config id)
         (nil? id) (throw (Exception. (str "Cant resolve nil")))
         :default id)]
    (println "**Resolve:" id "=>" result)
    result))



#_(def resolve (memoize resolve))

(defn -main
  [config-path & args]
  (let [base-dir  (.getParent (io/file config-path))
        config (read-config config-path)
        sounds (:sounds config)
        sounds (assoc sounds :base-dir base-dir)
        final-kw (:final config)
        final (resolve sounds final-kw)]
    (println "duration:" (duration final))
    (save final (relative-path (episode-file-name config) base-dir)  16000)))

(-main "../t1/episode.edn")

