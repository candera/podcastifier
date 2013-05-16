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

