(defproject org.craigandera/podcastifier "0.1.0-SNAPSHOT"
  :description "Automates audio mixing of podcast elements into the final product"
  ;; :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.namespace "0.2.3"]
                 [incanter "1.4.0"]
                 [com.googlecode.soundlibs/mp3spi "1.9.5-1"]]
  :java-source-paths ["java"]
  :main podcastifier.main
  :profiles {:dev
             {:source-paths ["dev"]
              :jvm-opts ["-Xdebug"
                         "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=9900"]}})
