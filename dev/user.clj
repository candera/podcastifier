(ns user
  "Holds utility functions for working at the REPL in this project"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [doc pst]]
            [clojure.tools.namespace.repl :refer [refresh]]
            [podcastifier.main :refer :all])
  (:import [javax.sound.sampled
            AudioSystem
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream]))

(defn init
  [])

(defn go
  []
  (init))

(defn stop
  [])

(defn reset
  []
  (stop)
  (refresh :after 'user/init))

(defn config
  []
  (-> "example-config.edn" io/reader java.io.PushbackReader. edn/read))

;; (defn main
;;   []
;;   (main/-main "example-config.edn"))

(defn sin-f
  [freq]
  (fn [t]
    [(-> t (* freq) (* Math/PI 2.0) Math/sin)]))

(defn play-f
  [f duration]
  (let [sample-rate 44000
        channels 1
        sdl (AudioSystem/getSourceDataLine (AudioFormat. sample-rate
                                                         16
                                                         channels
                                                         true
                                                         true))
        buffer-bytes (* sample-rate channels) ;; Half-second
        bb (java.nio.ByteBuffer/allocate buffer-bytes)
        total-bytes (-> duration (* sample-rate) long (* channels 2))]
    (.open sdl)
    (.start sdl)
    (loop [current-byte 0]
      (when (< current-byte total-bytes)
        (let [bytes-remaining (- total-bytes current-byte)
              bytes-to-write (min bytes-remaining buffer-bytes)]
          (.position bb 0)
          (doseq [i (range 0 (/ bytes-to-write 2 channels))]
            (.putShort bb (-> i
                              (+ current-byte)
                              double
                              (/ sample-rate channels 2)
                              f
                              first
                              (* Short/MAX_VALUE))))
          (recur (+ current-byte (.write sdl (.array bb) 0 bytes-to-write))))))))


(defn play-f2
  [f duration]
  (let [sample-rate  44000
        channels     1
        sdl          (AudioSystem/getSourceDataLine (AudioFormat. sample-rate
                                                                  16
                                                                  channels
                                                                  true
                                                                  true))
        buffer-bytes (* sample-rate channels) ;; Half-second
        bb           (java.nio.ByteBuffer/allocate buffer-bytes)
        total-bytes  (-> duration (* sample-rate) long (* channels 2))
        byte->t      (fn [n] (-> n double (/ sample-rate channels 2)))]
    (.open sdl)
    ;;(.start sdl)
    (loop [current-byte 0]
      (when (< current-byte total-bytes)
        (let [bytes-remaining (- total-bytes current-byte)
              bytes-to-write (min bytes-remaining buffer-bytes)]
          (.position bb 0)
          (doseq [i (range 0 bytes-to-write (* 2 channels))]
            (let [frame (f (byte->t (+ current-byte i)))]
              (doseq [samp frame]
                (.putShort bb (short-sample samp)))))
          (let [bytes-written (.write sdl (.array bb) 0 bytes-to-write)]
            (.start sdl)                ; Repeated calls are harmless
            (recur (+ current-byte bytes-written))))))))

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

(def s1 (read-sound "bumper.wav"))
(def s2 (read-sound "bumper-music.mp3"))
(def s3 (read-sound "C:/audio/craig/Led Zeppelin/Led Zeppelin II/01 Whole Lotta Love.mp3"))
(def s4 (sinusoid 1.0 440))


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
