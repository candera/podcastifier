(ns user
  "Holds utility functions for working at the REPL in this project"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [doc pst]]
            [clojure.tools.namespace.repl :refer [refresh]]
            [podcastifier.main :as main]))

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

(defn commands
  []
  (main/commands (-> "example-config.edn" io/reader java.io.PushbackReader. edn/read)))

(defn main
  []
  (main/-main "example-config.edn"))

