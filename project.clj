(defproject org.craigandera/podcastifier "0.2.0-SNAPSHOT"
  :description "Automates audio mixing of podcast elements into the final product"
  ;; :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.craigandera/dynne "0.3.0-SNAPSHOT"]
                 [prismatic/hiphip "0.1.0"]]
  :main podcastifier.main
  :profiles {:dev
             {:source-paths ["dev"]
              :dependencies [[org.clojure/tools.namespace "0.2.3"]]
              :jvm-opts ^:replace ["-Xdebug"
                                   ;;"-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=9900"
                                   ]
              }})
