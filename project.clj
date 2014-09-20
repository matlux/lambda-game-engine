(defproject connect4 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/algo.monads "0.1.4"]
                 [org.clojure/math.numeric-tower "0.0.3"]
                 [org.clojure/tools.trace "0.7.5"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]
                 [clojail "1.0.6"]]
;;  :main zone.lambda.game.connect4.core
  :main zone.lambda.game.main
  :jvm-opts ["-Djava.security.policy=java-security.policy"])
