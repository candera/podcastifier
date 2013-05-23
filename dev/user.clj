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

(defn read-frames
  "Reads and discards `n` frames from AudioInputStream `ais`"
  [^AudioInputStream ais n]
  (let [bytes-per-frame (-> ais .getFormat .getFrameSize)
        discard-frame-max 1000
        discard-buffer-bytes (* bytes-per-frame discard-frame-max)
        discard-buffer (byte-array discard-buffer-bytes)]
    (loop [total-frames-read 0]
      (let [frames-left-to-read (- n total-frames-read)]
        (when (pos? frames-left-to-read)
          (let [frames-to-read (min discard-frame-max frames-left-to-read)
                bytes-to-read (* bytes-per-frame frames-to-read)
                bytes-read (.read ais discard-buffer (int 0) (int bytes-to-read))
                frames-read (/ bytes-read bytes-per-frame)]
            (when (neg? frames-read)
              (throw (ex-info "Unexpected end of stream"
                              {:reason :unexpected-end-of-stream
                               :frames-read frames-read})))
            (recur (+ total-frames-read frames-read))))))))

(defn read-sound
  "Returns a Sound for the file at `path`."
  [path]
  (let [in                      (atom (AudioSystem/getAudioInputStream (io/file path)))
        base-format             (.getFormat @in)
        bits-per-sample         16
        bytes-per-sample        (/ bits-per-sample 8)
        channels                (.getChannels base-format)
        bytes-per-frame          (* bytes-per-sample channels)
        frames-per-second       (.getSampleRate base-format)
        decoded-format          (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                              frames-per-second
                                              bits-per-sample
                                              channels
                                              (* bytes-per-sample channels)
                                              frames-per-second
                                              true)
        din                     (atom (AudioSystem/getAudioInputStream decoded-format @in))
        decoded-length-seconds  (/ (.getFrameLength @din) frames-per-second)
        buffer-seconds          10
        buffer                  (byte-array (* frames-per-second
                                               buffer-seconds
                                               channels
                                               bytes-per-sample))
        starting-buffer-pos     [-1 -1]
        buffer-pos              (atom starting-buffer-pos)
        bb                      (java.nio.ByteBuffer/allocate bytes-per-frame)]
    (reify
      Sound
      (duration [s] decoded-length-seconds)
      (sample [s t]
        (if-not (<= 0.0 t decoded-length-seconds)
          (vec (repeat channels 0.0))
          (let [frame-at-t (-> t (* frames-per-second) long)]
            ;;(println "buffer-pos" @buffer-pos)

            ;; Desired frame is before current buffer. Reset everything
            ;; to the start state
            (when (< frame-at-t (first @buffer-pos))
              (println "rewinding")
              (.close @din)
              (.close @in)
              (reset! in (AudioSystem/getAudioInputStream (io/file path)))
              (reset! din (AudioSystem/getAudioInputStream decoded-format @in))
              (reset! buffer-pos starting-buffer-pos))

            ;; Desired position is past the end of the buffered region.
            ;; Update buffer to include it.
            (when (< (second @buffer-pos) frame-at-t)
              ;; We can't skip, because there's state built up during .read
              (println "Advancing" frame-at-t "frames")
              (read-frames @din (dec frame-at-t))
              (let [bytes-read (.read @din buffer)]
                (if (pos? bytes-read)
                  (let [frames-read (/ bytes-read bytes-per-frame)]
                    (reset! buffer-pos [frame-at-t (+ frame-at-t frames-read)]))
                  (reset! buffer-pos starting-buffer-pos))))

            ;; Now we're either positioned or the requested position
            ;; cannot be found
            (let [[buffer-start-pos buffer-end-pos] @buffer-pos]
              (if (neg? buffer-end-pos)
                (vec (repeat channels 0))
                (let [buffer-frame-offset (- frame-at-t buffer-start-pos)
                      buffer-byte-offset (* buffer-frame-offset bytes-per-frame)]
                  (.position bb 0)
                  (.put bb buffer buffer-byte-offset bytes-per-frame)
                  (.position bb 0)
                  ;; TODO: We're hardcoded to .getShort here, but the
                  ;; bits-per-frame is a parameter. Should probably have
                  ;; something that knows how to read from a ByteBuffer
                  ;; given a number of bits.
                  (vec (repeatedly channels #(/ (double (.getShort bb)) (inc Short/MAX_VALUE))))))))))

      java.io.Closeable
      (close [this]
        (.close @din)
        (.close @in)))))


;; #_(defn read-sound
;;   "Returns a sound for the file at `path`."
;;   [path]
;;   (let [frames-per-second 44100
;;         ;; Oddly, if we use 32-bit sound, we lose stereo. TODO: See if
;;         ;; we can get to 24 bits.
;;         bits-per-sample 16
;;         original-file (io/file path)
;;         original-input-stream ^java.io.InputStream (random-access-file-input-stream path)
;;         original-file-format (AudioSystem/getAudioFileFormat original-file)
;;         original-format (.getFormat original-file-format)
;;         original-encoding (-> original-format .getEncoding str)
;;         original-frame-rate (.getFrameRate original-format)
;;         original-file-duration (-> original-file-format
;;                                    .getFrameLength
;;                                    (/ original-frame-rate))
;;         channels (.getChannels original-format)
;;         conversion-ais (atom (conversion-audio-input-stream original-input-stream
;;                                                             frames-per-second
;;                                                             bits-per-sample))
;;         conversion-format (.getFormat @conversion-ais)
;;         bytes-per-frame (.getFrameSize conversion-format)
;;         bytes-per-second (* bytes-per-frame frames-per-second)
;;         scale-value (Math/pow 2 (dec bits-per-sample))
;;         cache-seconds 10
;;         cache-buf (byte-array (* bytes-per-frame frames-per-second cache-seconds))
;;         cache (atom [0 -1])
;;         bb (java.nio.ByteBuffer/allocate bytes-per-frame)]
;;     (.mark original-input-stream 0)
;;     (reify
;;       Sound
;;       (duration [this] original-file-duration)
;;       (sample [this t]
;;         (let [frame-num (-> t (* frames-per-second) long)
;;               byte-num (* frame-num bytes-per-frame)
;;               [cache-frame-start cache-frame-end] @cache]
;;           (when (< frame-num cache-frame-start)
;;             ;; Rewind to beginning and populate cache
;;             (println "Resetting")
;;             (.reset original-input-stream)
;;             (reset! conversion-ais (conversion-audio-input-stream original-input-stream
;;                                                                   frames-per-second
;;                                                                   bits-per-sample))
;;             ;; TODO: Does this need to be .read instead?
;;             (.skip ^AudioInputStream @conversion-ais byte-num)
;;             (let [bytes-read (.read ^AudioInputStream @conversion-ais cache-buf)
;;                   frames-read (/ bytes-read bytes-per-frame)
;;                   last-frame (dec (+ frame-num frames-read))]
;;               (reset! cache [frame-num last-frame])))

;;           (when (< cache-frame-end frame-num)
;;             ;; Advance to position cache at desired frame
;;             (let [frames-to-skip (- frame-num cache-frame-end 1)]
;;               ;;(println "Advancing" frames-to-skip "frames")
;;               (.skip ^AudioInputStream @conversion-ais (* bytes-per-frame frames-to-skip)))
;;             (let [bytes-read (.read ^AudioInputStream @conversion-ais cache-buf)
;;                   frames-read (/ bytes-read bytes-per-frame)
;;                   last-frame (dec (+ frame-num frames-read))]
;;               #_(when-not (= (long frames-read) frames-read)
;;                 (throw (ex-info "non-integral number of frames read"
;;                                 {:reason :non-integral-frame-read
;;                                  :frames frames-read})))
;;               (println "read" bytes-read)
;;               (if (neg? bytes-read)
;;                 (throw (ex-info "Unexpectedly hit end of input stream"
;;                                 {:reason :unexpected-eos
;;                                  :frame-num frame-num
;;                                  :cache-start cache-frame-start
;;                                  :cache-end cache-frame-end}))
;;                 (reset! cache [frame-num last-frame]))))

;;           (.position bb 0)
;;           (.put bb cache-buf (* (- frame-num (first @cache))) bytes-per-frame)
;;           (.position bb 0)
;;           (vec (repeatedly channels #(/ (.getShort bb) scale-value)))))

;;       java.io.Closeable
;;       (close [this]
;;         (.close @conversion-ais)
;;         ;;(.close original-ais)
;;         (.close original-input-stream)))))

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
