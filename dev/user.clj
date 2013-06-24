(ns user
  "Holds utility functions for working at the REPL in this project"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [doc pst]]
            [clojure.tools.namespace.repl :refer [refresh]]
            [dynne.sound :refer :all]
            [podcastifier.main :refer :all])
  (:import [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem]))

(defn init
  [])

(defn go
  []
  (init))

(defn user-stop
   [])

(defn reset
  []
  (user-stop)
  (refresh :after 'user/init))

(defn config
  []
  (-> "example-config.edn" io/reader java.io.PushbackReader. edn/read))

;; (defn main
;;   []
;;   (main/-main "example-config.edn"))

;;; Reading from files

(defn random-access-file-input-stream
  "Given `path` returns a java.io.InputStream over it that supports
  random access via mark and reset."
  [path]
  (let [raf (java.io.RandomAccessFile. (io/file path) "r")
        marked-position (atom nil)]
   (proxy [java.io.InputStream] []
     (available [] (- (.length raf) (.getFilePointer raf)))
     (close [] (.close raf))
     (mark [readlimit] (reset! marked-position (.getFilePointer raf)))
     (markSupported [] true)
     (read ^int
       ([] (.read raf))
       ([buf] (.read raf buf))
       ([^bytes buf off len] (.read raf buf off len)))
     (reset [] (.seek raf @marked-position))
     (skip [n] (.skipBytes raf n)))))

(defn conversion-audio-input-stream
  "Returns an AudioInputStream on top of a java.io.InputStream."
  [^java.io.InputStream original-input-stream frames-per-second bits-per-sample]
  (let [original-ais (AudioSystem/getAudioInputStream original-input-stream)
        original-format (.getFormat original-ais)
        channels (.getChannels original-format)
        decoded-format (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                     (.getSampleRate original-format)
                                     bits-per-sample
                                     channels
                                     (* (/ bits-per-sample 8) channels)
                                     (.getSampleRate original-format)
                                     true)]
    (AudioSystem/getAudioInputStream
     decoded-format
     original-ais)))

(comment
  (def s1 (read-sound "bumper.wav"))
  (def s2 (read-sound "bumper-music.mp3"))
  (def s3 (read-sound "C:/audio/craig/Led Zeppelin/Led Zeppelin II/01 Whole Lotta Love.mp3"))
  (def s4 (sinusoid 1.0 440))

  (def s440 (sinusoid 30 440))
  (def s880 (sinusoid 30 880))

  (def sin2ch (->BasicSound 30 (fn [t] [(first (sample s440 t)) (first (sample s880 t))])))

  (def s5 (fade s2 [[1.0 0] [0.01 10]])))

(defn decoded-ais
  [path]
  (let [in (AudioSystem/getAudioInputStream (io/file path))
        baseFormat (.getFormat in)
        decodedFormat (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                        (.getSampleRate baseFormat)
                                        16
                                        (.getChannels baseFormat)
                                        (* 2 (.getChannels baseFormat))
                                        (.getSampleRate baseFormat)
                                        false)]
    (AudioSystem/getAudioInputStream decodedFormat in)))

(defn roundtrip
  [in out]
  (let [d (decoded-ais in)]
    (AudioSystem/write ^AudioInputStream d
                       AudioFileFormat$Type/WAVE
                       (io/file out))))

(defn consume
  "Rip through an AudioInputStream, consuming buflen bytes at a time, if possible"
  [^AudioInputStream ais buflen]
  (let [buffer (byte-array buflen)]
    (while (not (neg? (.read ais buffer))))))



;; (defn zeros
;;   "Returns an infinite seq of the times where `s` is within `epsilon` of zero."
;;   [s epsilon]
;;   (let [channels (channels s)]
;;     (->> (iterate #(+ % (/ 1 44100.0)) 0.0)
;;          (map (fn [t] [t (sample s t)]))
;;          (filter (fn [[t samp]] (every? #(< (Math/abs ^double %) epsilon) samp)))
;;          (map first))))


(def config (read-config "episode.edn"))

;; (def voices-config (:voices config))
;; (def intro-config (-> config :music :intro))


;; (def i (intro-music voices-config intro-config))
;; (def v (voices voices-config))

;; (def v+i
;;   (-> v
;;       (timeshift (-> intro-config :full-volume-length))
;;       (mix i)))

(defn oversample-all
  "Oversample all of s, for performance measurement purposes."
  [s]
  (let [inc-t (/ 1.0 44100)
        delta-t (/ inc-t 4)
        max-t (duration s)]
    (time
     (loop [t 0.0]
       (when (< t max-t)
         ;;(sample s t)
         (oversample4 s t 0 delta-t)
         (recur (+ t inc-t)))))))

;; File file = new File(filename);
;; AudioInputStream in= AudioSystem.getAudioInputStream(file);
;; AudioInputStream din = null;
;; AudioFormat baseFormat = in.getFormat();
;; AudioFormat decodedFormat = new AudioFormat(AudioFormat.Encoding.PCM_SIGNED,
;;                                             baseFormat.getSampleRate(),
;;                                             16,
;;                                             baseFormat.getChannels(),
;;                                             baseFormat.getChannels() * 2,
;;                                             baseFormat.getSampleRate(),
;;                                             false);
;; din = AudioSystem.getAudioInputStream(decodedFormat, in);

(comment

(def file (io/file "C:/audio/craig/Led Zeppelin/Led Zeppelin II/01 Whole Lotta Love.mp3"))
(def file (io/file "bumper.wav"))
(def in (AudioSystem/getAudioInputStream file))
(def baseFormat (.getFormat in))
(def decodedFormat (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                 (.getSampleRate baseFormat)
                                 16
                                 (.getChannels baseFormat)
                                 (* 2 (.getChannels baseFormat))
                                 (.getSampleRate baseFormat)
                                 false))


(def din (AudioSystem/getAudioInputStream decodedFormat in))

(def cin (conversion-audio-input-stream (random-access-file-input-stream "bumper.wav")
                                        (.getSampleRate baseFormat)
                                        16))



(let [buf (byte-array (* 2 (.getChannels baseFormat)))]
  (time (while (pos? (.read ^AudioInputStream cin buf)))))

;; (AudioSystem/write din javax.sound.sampled.AudioFileFormat$Type/WAVE (io/file "C:/temp/stereo-out.wav"))


(time (doseq [t (range 0 (duration s3) 1/44100)] (sample s3 t)))


(def path "bumper.wav")
(def path "bumper-music.mp3")
(def frames-per-second 44100)
;; Oddly, if we use 32-bit sound, we lose stereo. TODO: See if
;; we can get to 24 bits.
(def bits-per-sample 16)
(def original-file (io/file path))
(def original-input-stream ^java.io.InputStream (random-access-file-input-stream path))
(def original-file-format (AudioSystem/getAudioFileFormat original-file))
(def original-format (.getFormat original-file-format))
(def original-encoding (-> original-format .getEncoding str))
(def original-frame-rate (.getFrameRate original-format))
(def original-file-duration (-> original-file-format .getFrameLength (/ original-frame-rate)))
(def channels (.getChannels original-format))
(def conversion-ais (atom (conversion-audio-input-stream original-input-stream frames-per-second bits-per-sample)))
(def conversion-format (.getFormat @conversion-ais))
(def bytes-per-frame (.getFrameSize conversion-format))
(def bytes-per-second (* bytes-per-frame frames-per-second))
(def scale-value (Math/pow 2 (dec bits-per-sample)))
(def cache-seconds 10)
(def cache-buf (byte-array (* bytes-per-frame frames-per-second cache-seconds)))
(def cache (atom [0 -1]))
(def bb (java.nio.ByteBuffer/allocate bytes-per-frame))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

(defn beeps
  "Returns a sound that beeps for n seconds at frequency freq"
  [freq n]
  (->> (cycle [(sinusoid 0.1 440) (silence 0.4)])
       (take (* 4 n))
       (reduce append)))

(def config (read-config "episode.edn"))

(def voices-config (:voices config))
(def intro-config (-> config :music :intro))
(def outro-config (-> config :music :outro))


(def i (intro-music voices-config intro-config))
(def v (voices voices-config))
(def o (outro-music voices-config outro-config))

(visualize  (-> v
                (mix (timeshift o (+ (:fade-in voices-config)
                                     (- (:outro-music-start voices-config)
                                        (:start voices-config)))))
                (timeshift (:full-volume-length intro-config))
                (mix i)))


(time (save (-main "episode.edn") "episode.wav" 44100))

)

;;; Experimentation with combining sound operations

(comment

(defop multiply
  [s1 s2]
  {:channels (channels s1)
   :duration (max (duration s1) (duration s2))
   :amplitude ([t c] (p/* (sample s1 t c) (sample s2 t c)))})

(compile (multiply s1 s2)) ;=>
(sound (max (duration s1) (duration s2))
       (fn [t c] (p/* (sample s1 t c) (sample s2 t c)))
       (channels s1))

(compile (multiply s1 s2) (multiply s3 s2)) ;=>

(sound (max (max duration s1 s2) (max duration s3 s2))
       (fn [t c] (p/* (p/* (sample s1 t c) (sample s2 t c))
                      (p/* (sample s3 t c) (sample s2 t c)))))

;; Better still
(sound (max (max duration s1 s2) (max duration s3 s2))
       (fn [t c] (let [samp2 (sample s2 t c)]
                   (p/* (p/* (sample s1 t c) samp2)
                        (p/* (sample s3 t c) samp2)))))

(defprotocol Compileable
  (compile []))

(defrecord IdentityOp [snd]
  Compileable
  (compile
    {:duration (duration snd)
     :channels (channels snd)
     :amplitude ([t c] (.amplitude snd t c))})
  )

;; Hmm. Maybe something simpler:

(defn ->op
  "Create an operation that just samples that sound
  without additional processing."
  [s]
  (let [s* (gensym "sound")]
   `{:bindings {~s* ~s}
     :duration (.duration ~s*)
     :channels (.channels ~s*)
     :amplitude (.amplitude ~s* ~'t ~'c)}))


(->op '(read-sound "foo.wav"))

;=>
{:duration (.duration sound7679)
 :bindings {sound7679 (read-sound "foo.wav")}
 :amplitude (.amplitude sound7679 t c)
 :channels (.channels sound7679)}

(defn gain-op [gain op]
  (update-in op [:amplitude]
             (fn [e] `(* ~gain ~e))))

(gain-op 0.5 (->op '(read-sound "foo.wav")))

;=>
{:duration (.duration sound7697),
 :bindings {sound7697 (read-sound "foo.wav")},
 :amplitude (clojure.core/* 0.5 (.amplitude sound7697 t c)),
 :channels (.channels sound7697)}


(defn multiply-op [op1 op2]
  {:bindings (merge (:bindings op1) (:bindings op2))
   :duration `(max ~(:duration op1) ~(:duration op2))
   :channels `(.channels s1)
   :amplitude `(* ~(:amplitude op1) ~(:amplitude op2))})


(multiply-op (gain-op 0.5 (->op 's1)) (->op 's2))

;=>

{:bindings {sound7727 s1, sound7728 s2},
 :duration
 (clojure.core/max (.duration sound7727) (.duration sound7728)),
 :channels (.channels user/s1),
 :amplitude
 (clojure.core/*
  (clojure.core/* 0.5 (.amplitude sound7727 t c))
  (.amplitude sound7728 t c))}

;; Now we just need something to take an operation and compile it into
;; a sound.
;; E.g.
(compile (multiply-op (->op (read-sound "hi.wav"))
                      (->op s2)))

;=>
(let [s1 (read-sound "hi.wav")]
  (sound (max (.duration s1) (.duration s2))
         (fn [t c] (* (.amplitude s1 c) (.amplitude s2 c)))
         (.channels s1))))
