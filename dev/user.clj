(ns user
  "Holds utility functions for working at the REPL in this project"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [doc pst]]
            [clojure.tools.namespace.repl :refer [refresh]]
            [podcastifier.main :refer :all])
  (:import [javax.sound.sampled AudioSystem AudioFormat]))

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
  [original-input-stream frames-per-second bits-per-sample]
  (let [original-ais (AudioSystem/getAudioInputStream original-input-stream)
        original-format (.getFormat original-ais)
        channels (.getChannels original-format)]
    (AudioSystem/getAudioInputStream
     (AudioFormat. frames-per-second bits-per-sample channels true true)
     original-ais)))

(defn read-sound
  "Returns a sound for the file at `path`."
  [path]
  (let [frames-per-second 44100
        bits-per-sample 32
        original-file (io/file path)
        original-input-stream ^java.io.InputStream (random-access-file-input-stream path)
        original-file-format (AudioSystem/getAudioFileFormat original-file)
        original-format (.getFormat original-file-format)
        original-encoding (-> original-format .getEncoding str)
        original-frame-rate (.getFrameRate original-format)
        original-file-duration (-> original-file-format
                                   .getFrameLength
                                   (/ original-frame-rate))
        channels (.getChannels original-format)
        conversion-ais (atom (conversion-audio-input-stream original-input-stream
                                                            frames-per-second
                                                            bits-per-sample))
        conversion-format (.getFormat @conversion-ais)
        bytes-per-frame (.getFrameSize conversion-format)
        bytes-per-second (* bytes-per-frame frames-per-second)
        scale-value (Math/pow 2 (dec bits-per-sample))
        ;; Bug workaround for weird double-speed playback of MP3 files
        time-conversion (if (= "MPEG1L3" original-encoding) 2.0 1.0)
        cache (atom [-1 nil])]
    (.mark original-input-stream 0)
    (reify
      Sound
      (duration [this] original-file-duration)
      (sample [this t]
        (let [frame-num (-> t (/ time-conversion) (* frames-per-second) long)
              byte-num (* frame-num bytes-per-frame)
              bb (java.nio.ByteBuffer/allocate bytes-per-frame)
              buf (byte-array bytes-per-frame)
              [last-frame-num last-frame] @cache]
          (if (= last-frame-num frame-num)
            last-frame
            (do
              (if (< last-frame-num frame-num)
                (let [bytes-to-skip (* bytes-per-frame (- frame-num last-frame-num 1))]
                  (when (pos? bytes-to-skip)
                    (.skip @conversion-ais bytes-to-skip)))
                (do
                  ;; We need to reopen the stream, because the MP3
                  ;; support doesn't let us rewind
                  (.reset original-input-stream)
                  (reset! conversion-ais (conversion-audio-input-stream original-input-stream
                                                                        frames-per-second
                                                                        bits-per-sample))
                  (.skip @conversion-ais byte-num)))
              (.read @conversion-ais buf)
              (.put bb buf)
              (.position bb 0)
              (let [frame (vec (repeatedly channels #(/ (.getInt bb) scale-value)))]
                (reset! cache [frame-num frame])
                frame)))))

      java.io.Closeable
      (close [this]
        (.close @conversion-ais)
        ;;(.close original-ais)
        (.close original-input-stream)))))

(def s1 (read-sound "bumper.wav"))
(def s2 (read-sound "bumper-music.mp3"))
(def s3 (read-sound "C:/audio/craig/Led Zeppelin/Led Zeppelin II/01 Whole Lotta Love.mp3"))